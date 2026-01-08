%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Douglas Triggs <doug@chef.io>
%% @author Mark Mzyk <mm@chef.io>
%% Copyright Chef Software, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

-module(oc_chef_wm_acl_permission).

-include("oc_chef_wm.hrl").

%% Webmachine resource callbacks
-mixin([{oc_chef_wm_base, [content_types_accepted/2,
                           content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           post_is_create/2,
                           forbidden/2,
                           is_authorized/2,
                           service_available/2]}]).
-export([allowed_methods/2,
         from_json/2]).

%% Username mapping functions for multi-tenancy support
-export([strip_tenant_id/1,
         append_tenant_id/2,
         transform_usernames_for_response/1,
         transform_usernames_for_request/2]).

%% chef_wm behaviour callbacks
-behaviour(chef_wm).
-export([
         auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3
        ]).


init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(Config) ->
    AclType = ?gv(acl_object_type, Config),
    {ok, #acl_state{type = AclType}}.

request_type() ->
    "acl".

allowed_methods(Req, State) ->
    {['PUT'], Req, State}.

-spec validate_request(chef_wm:http_verb(), wm_req(), chef_wm:base_state()) ->
                              {wm_req(), chef_wm:base_state()}.
validate_request('PUT', Req, #base_state{chef_db_context = DbContext,
                                         organization_guid = OrgId,
                                         resource_state = #acl_state{type = Type} =
                                             AclState} = State) ->
    Body = wrq:req_body(Req),
    Ace = chef_json:decode_body(Body),
    Part = list_to_binary(wrq:path_info(acl_permission, Req)),

    %% Transform usernames: append tenant IDs to "users" list
    TenantId = case wrq:get_req_header("x-ops-tenantid", Req) of
        undefined -> undefined;
        HeaderValue -> list_to_binary(HeaderValue)
    end,

    TransformedAce = case Ace of
        {PropList} ->
            %% Transform the permission part (create/read/update/delete/grant)
            TransformedProps = lists:map(fun
                ({PartKey, {PartProps}}) when PartKey == Part ->
                    %% Transform nested "users" and "actors" arrays for username mapping
                    TransformedPartProps = lists:map(fun
                        ({<<"users">>, Users}) when is_list(Users) ->
                            {<<"users">>, transform_usernames_for_request(Users, TenantId)};
                        ({<<"actors">>, Actors}) when is_list(Actors) ->
                            {<<"actors">>, transform_usernames_for_request(Actors, TenantId)};
                        (Other) ->
                            Other
                    end, PartProps),
                    {PartKey, {TransformedPartProps}};
                (Other) ->
                    Other
            end, PropList),
            {TransformedProps};
        _ ->
            Ace
    end,

    %% Make sure we have valid json before trying other checks
    %% Throws if invalid json is found
    check_json_validity(Part, TransformedAce),

    %% validate_authz_id will populate the ACL AuthzId, which is needed
    %% for checking the ACL constraints; we need to run validate_authz_id
    %% anyway, so go ahead and do so
    {Req1, State1 = #base_state{resource_state = #acl_state{
          authz_id = AuthzId}}} =
                                  oc_chef_wm_acl:validate_authz_id(Req, State,
                                                                   AclState#acl_state{acl_data = TransformedAce},
                                                                    Type, OrgId,  DbContext),

    %% Check if we're violating any constraints around modifying ACLs
    %% i.e. deleting default groups, etc.

    case oc_chef_wm_base:is_superuser(Req) of
      true ->
        %% It's the superuser; they are allowed to bypass the checks, assuming
        %% they know what they are doing. With great power and all.
        {Req1, State1};
      false ->
        case oc_chef_authz_acl_constraints:check_acl_constraints(OrgId, AuthzId, Type, Part, TransformedAce) of
          ok ->
            {Req1, State1};
          [ Violation | _T ] ->
            %% Received one or more failures. Report back the first one.
            throw({acl_constraint_violation, Violation})
        end
    end.

auth_info(Req, State) ->
    oc_chef_wm_acl:check_acl_auth(Req, State).

from_json(Req, #base_state{organization_guid = OrgId,
                           resource_state = AclState} = State) ->
    Part = wrq:path_info(acl_permission, Req),
    construct_return(update_from_json(AclState, Part, OrgId), OrgId, Req, State).

construct_return(forbidden, _OrgId, Req, State) ->
    {{halt, 400}, Req, State};
construct_return({ambiguous_actor, Actors}, _OrgId, Req, State) ->
    Message = [<<"The following actor(s) exist as both clients and users within this organization: ">>,
               chef_wm_malformed:bin_str_join(Actors, ", "),
               <<". To perform this update, supply separate 'clients' and 'users' ">>,
               <<"fields in your request, and use an empty array for the value of 'actors'.">>],
    Body = chef_wm_util:error_message_envelope(Message),
    Req1 = wrq:set_resp_body(chef_json:encode(Body), Req),
    {{halt, 422}, Req1, State#base_state{log_msg = {ambiguous_actor, Actors}}};
construct_return({invalid, client, Names}, undefined, Req, State) ->
    Body = chef_wm_util:error_message_envelope([<<"Clients can not be added to global ACL entries.">>]),
    Req1 = wrq:set_resp_body(chef_json:encode(Body), Req),
    {{halt, 400}, Req1, State#base_state{log_msg = {invalid_object_in_ace, Names}}};
construct_return({invalid, user, Names}, undefined, Req, State) ->
    Body = case [ extract_full_name(N) || N <- Names ] of
               [SingleName] ->
                   chef_wm_util:error_message_envelope([<<"The user ">>,
                                                        chef_wm_malformed:bin_str_join([SingleName], ", "),
                                                        <<" does not exist.">>]);
               UserList ->
                   chef_wm_util:error_message_envelope([<<"The users ">>,
                                                        chef_wm_malformed:bin_str_join(UserList, ", "),
                                                        <<" do not exist.">>])
           end,
    Req1 = wrq:set_resp_body(chef_json:encode(Body), Req),
    {{halt, 400}, Req1, State#base_state{log_msg = {invalid_object_in_ace, Names}}};
construct_return({invalid, Type, Names}, _OrgId, Req, State) ->
    FullNames = [ extract_full_name(N) || N <- Names ],
    Body = chef_wm_util:error_message_envelope([<<"The ">>, atom_to_list(Type),
                                                <<"(s) ">>, chef_wm_malformed:bin_str_join(FullNames, ", "),
                                                <<" do not exist in this organization.">>]),
    Req1 = wrq:set_resp_body(chef_json:encode(Body), Req),
    {{halt, 400}, Req1, State#base_state{log_msg = {invalid_object_in_ace, Names}}};
construct_return({bad_actor, Actors}, undefined, Req, State) ->
    Body = chef_wm_util:error_message_envelope([<<"The actor(s) ">>,
                                                chef_wm_malformed:bin_str_join(Actors, ", "),
                                                <<" is not a valid user (clients are not allowed in this context).">>]),
    Req1 = wrq:set_resp_body(chef_json:encode(Body), Req),
    {{halt, 400}, Req1, State#base_state{log_msg = {bad_actor, Actors}}};
construct_return({bad_actor, Actors}, _OrgId, Req, State) ->
    Body = chef_wm_util:error_message_envelope([<<"The actor(s) ">>,
                                                chef_wm_malformed:bin_str_join(Actors, ", "),
                                                <<" do not exist in this organization as clients or users.">>]),
    Req1 = wrq:set_resp_body(chef_json:encode(Body), Req),
    {{halt, 400}, Req1, State#base_state{log_msg = {bad_actor, Actors}}};
construct_return({inappropriate_scoped_name, Actors}, _OrgId, Req, State) ->
    Body = chef_wm_util:error_message_envelope([<<"Scoped actor names are not allowed in this context: ">>,
                                                chef_wm_malformed:bin_str_join(Actors, ", ")]),
    Req1 = wrq:set_resp_body(chef_json:encode(Body), Req),
    {{halt, 400}, Req1, State#base_state{log_msg = {bad_actor, Actors}}};
construct_return(_Other, _OrgId, Req, State) ->
    %% So we return 200 instead of 204, for backwards compatibility:
    Req1 = wrq:set_resp_body(<<"{}">>, Req),
    {true, Req1, State}.


%%
%% Internal functions
%%

%% Extracts the full_name from a record, only if they are not binary() or list()
extract_full_name(N) when is_binary(N) orelse is_list(N) ->
    N;
extract_full_name(N) ->
    oc_chef_authz_scoped_name:full_name(N).

check_json_validity(Part, Ace) ->
  case chef_object_base:strictly_valid(oc_chef_authz_acl:acl_spec(Part), [Part], Ace) of
    ok ->
      ok;
    Other ->
      throw(Other)
  end,
  oc_chef_authz_acl:validate_actors_clients_users(Part, Ace).

update_from_json(#acl_state{type = Type, authz_id = AuthzId, acl_data = Data},
                 Part, OrgId) ->
    try
        oc_chef_authz_acl:update_part(Part, Data, Type, AuthzId, OrgId)
    catch
        throw:{ambiguous_actor, Actors} ->
            {ambiguous_actor, Actors};
        throw:{inappropriate_scoped_name, Actors} ->
            {inappropriate_scoped_name, Actors};
        throw:forbidden ->
            forbidden;
        throw:{bad_actor, Actors} ->
            {bad_actor, Actors};
        throw:{invalid, T1, Names} ->
            {invalid, T1, Names}
    end.

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

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
%% Superusers are exempt from tenant mapping as they operate globally.
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
