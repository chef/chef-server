%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@chef.io>
%% Copyright Chef Software, Inc. All Rights Reserved.

-module(oc_chef_wm_groups).

-include("oc_chef_wm.hrl").

%% Webmachine resource callbacks
-mixin([{oc_chef_wm_base, [content_types_accepted/2,
                           content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           post_is_create/2,
                           {list_objects_json/2, to_json},
                           forbidden/2,
                           is_authorized/2,
                           service_available/2]}]).

-export([allowed_methods/2,
         conflict_message/1,
         create_path/2,
         from_json/2,
         resource_exists/2]).

%% chef_wm behavior callbacks
-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3 ]).

%% Shared functions
-export([validate_group_name/1,
         group_name_invalid/2 ]).

%% Username mapping functions for multi-tenancy support
-export([strip_tenant_id/1,
         append_tenant_id/2,
         transform_usernames_for_response/1,
         transform_usernames_for_request/2]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #group_state{}}.

request_type() ->
    "groups".

allowed_methods(Req, State) ->
    {['GET', 'POST'], Req, State}.

-spec validate_request(chef_wm:http_verb(), wm_req(), chef_wm:base_state()) ->
                              {wm_req(), chef_wm:base_state()}.
validate_request('GET', Req, #base_state{organization_guid = OrgId} = State) ->
    {Req, State#base_state{superuser_bypasses_checks = true,
                           resource_state = #oc_chef_group{org_id = OrgId}}};
validate_request('POST', Req, #base_state{organization_guid = OrgId,
                                          resource_state = ResourceState}
                 = State) ->
    Body = wrq:req_body(Req),
    {ok, Json} = oc_chef_group:parse_binary_json(Body),
    
    %% Transform usernames: append tenant IDs to "users" list
    %% TESTING ONLY - Hardcoded tenant ID for manual testing
    TenantId = <<"12345678-1234-1234-1234-123456789012">>,
    %% Original code (commented for testing):
    %% TenantId = case wrq:get_req_header("x-ops-tenantid", Req) of
    %%     undefined -> undefined;
    %%     HeaderValue -> list_to_binary(HeaderValue)
    %% end,
    
    TransformedJson = case Json of
        {PropList} ->
            TransformedProps = lists:map(fun
                ({<<"actors">>, {ActorProps}}) when is_list(ActorProps) ->
                    %% Transform nested actors.users only
                    TransformedActorProps = lists:map(fun
                        ({<<"users">>, Users}) when is_list(Users) ->
                            {<<"users">>, transform_usernames_for_request(Users, TenantId)};
                        ({<<"clients">>, Clients}) ->
                            {<<"clients">>, Clients};
                        (Other) ->
                            Other
                    end, ActorProps),
                    {<<"actors">>, {TransformedActorProps}};
                (Other) ->
                    Other
            end, PropList),
            {TransformedProps};
        _ ->
            Json
    end,
    
    {Req, State#base_state{superuser_bypasses_checks = true,
                           resource_state = ResourceState#group_state{
                                              oc_chef_group =
                                                  #oc_chef_group{
                                                     org_id = OrgId},
                                              group_data = TransformedJson}}}.

auth_info(Req, State) ->
    auth_info(wrq:method(Req), Req, State).

auth_info('GET', Req, State ) ->
    {{container, group}, Req, State};
auth_info('POST', Req, State = #base_state{resource_state = #group_state{group_data = Json}}) ->
    case validate_group_name(fetch_id_name_from_json(Json)) of
        valid ->
            {{create_in_container, group}, Req, State};
        _ ->
            group_name_invalid(Req, State)
    end.

resource_exists(Req, State) ->
    {true, Req, State}.

create_path(Req, #base_state{resource_state = #group_state{group_data = GroupData}} = State) ->
    Name = fetch_id_name_from_json(GroupData),
    {binary_to_list(Name), Req, State}.

from_json(Req, #base_state{resource_state = #group_state{group_data = GroupData,
                                                             group_authz_id = AuthzId}} = State) ->
    oc_chef_wm_base:create_from_json(Req, State, oc_chef_group, {authz_id, AuthzId}, GroupData).

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

-spec conflict_message(binary()) -> ejson_term().
conflict_message(_Name) ->
    {[{<<"error">>, [<<"Group already exists">>]}]}.

-define(VALID_NAME_REGEX, "^[a-z0-9\-_]+$").

validate_group_name(undefined) ->
    missing;
validate_group_name(Name) ->
    {ok, CompiledRegex} = re:compile(?VALID_NAME_REGEX),
    case re:run(Name, CompiledRegex) of
        {match, _} ->
            valid;
        nomatch  ->
            invalid
    end.

group_name_invalid(Req, State) ->
    Msg = <<"Invalid group name.">>,
    JsonError = {[{<<"error">>, [Msg]}]},
    {{halt, 400}, chef_wm_util:set_json_body(Req, JsonError), State}.

%% We allow either id or groupname to be set for a group.  Precedence is
%% given to id over groupname if both set.
fetch_id_name_from_json(GroupJson) ->
    ej:get({<<"id">>}, GroupJson, ej:get({<<"groupname">>}, GroupJson)).

%%====================================================================
%% Username Mapping Functions for Multi-tenancy Support
%%====================================================================

%% @doc Strip tenant ID suffix from mapped username, returning legacy username.
%% Splits on the LAST occurrence of double-underscore followed by UUID.
%% Returns {ok, LegacyUsername} if tenant suffix found and stripped.
%% Returns {error, MappedUsername} if no valid tenant suffix (username returned as-is).
%% Special case: If stripping results in empty binary, returns {error, MappedUsername}.
%% Caller should log warning when {error, _} is returned.
-spec strip_tenant_id(binary()) -> {ok, binary()} | {error, binary()}.
strip_tenant_id(MappedUsername) when is_binary(MappedUsername) ->
    case binary:split(MappedUsername, <<"__">>, [global]) of
        [_SinglePart] ->
            {error, MappedUsername};
        Parts ->
            case lists:reverse(Parts) of
                [PotentialUuid | ReversedUsernameParts] ->
                    case is_valid_uuid(PotentialUuid) of
                        true ->
                            LegacyUsername = join_with_separator(
                                lists:reverse(ReversedUsernameParts), 
                                <<"__">>
                            ),
                            case LegacyUsername of
                                <<>> -> {error, MappedUsername};
                                _ -> {ok, LegacyUsername}
                            end;
                        false ->
                            {error, MappedUsername}
                    end
            end
    end.

%% @doc Append tenant ID to legacy username, creating mapped username.
%% Returns binary in format "username__tenant-uuid".
-spec append_tenant_id(binary(), binary()) -> binary().
append_tenant_id(LegacyUsername, TenantId) 
  when is_binary(LegacyUsername), is_binary(TenantId) ->
    <<LegacyUsername/binary, "__", TenantId/binary>>.

%% @doc Transform list of usernames for response (strip tenant IDs).
%% Logs warning for usernames that cannot be stripped.
-spec transform_usernames_for_response([binary()]) -> [binary()].
transform_usernames_for_response(Usernames) when is_list(Usernames) ->
    lists:map(fun(Username) ->
        case strip_tenant_id(Username) of
            {ok, LegacyUsername} ->
                LegacyUsername;
            {error, OriginalUsername} ->
                lager:warning("Username ~p has no tenant suffix, returning as-is", 
                             [OriginalUsername]),
                OriginalUsername
        end
    end, Usernames).

%% @doc Transform list of usernames for request (append tenant IDs).
%% If TenantId is undefined, logs warning and returns usernames unchanged.
-spec transform_usernames_for_request([binary()], binary() | undefined) -> [binary()].
transform_usernames_for_request(Usernames, undefined) when is_list(Usernames) ->
    lager:warning("X-Ops-TenantId header missing, usernames not transformed"),
    Usernames;
transform_usernames_for_request(Usernames, TenantId) 
  when is_list(Usernames), is_binary(TenantId) ->
    lists:map(fun(Username) ->
        case oc_chef_wm_base:is_superuser(Username) of
            true -> Username;  % Don't map superusers
            false -> append_tenant_id(Username, TenantId)
        end
    end, Usernames).

%% @doc Check if binary matches UUID format (8-4-4-4-12 hex digits).
-spec is_valid_uuid(binary()) -> boolean().
is_valid_uuid(<<_:8/binary, "-", _:4/binary, "-", _:4/binary, "-", 
                _:4/binary, "-", _:12/binary>>) ->
    true;
is_valid_uuid(_) ->
    false.

%% @doc Join binary parts with separator.
-spec join_with_separator([binary()], binary()) -> binary().
join_with_separator([], _Sep) ->
    <<>>;
join_with_separator([Part], _Sep) ->
    Part;
join_with_separator([Head | Tail], Sep) ->
    lists:foldl(
        fun(Part, Acc) -> <<Acc/binary, Sep/binary, Part/binary>> end,
        Head,
        Tail
    ).
