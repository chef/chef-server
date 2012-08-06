%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @author Seth Falcon <seth@opscode.com>
%% @copyright 2012 Opscode, Inc.

-module(oc_chef_wm_base).

%% Complete webmachine callbacks
-export([forbidden/2,
         is_authorized/2,
         service_available/2]).

%% Helpers for webmachine callbacks
-export([authorized_by_org_membership_check/2,
         delete_object/3]).

%% Can't use callback specs to generate behaviour_info because webmachine.hrl
%% contains a function definition.

%% -callback validate_request(atom(), #wm_reqdata{}, any()) -> {#wm_reqdata{}, any()}.
%% -callback malformed_request_message(any(), #wm_reqdata{}, any()) -> {[{binary(), [binary()]}]}.
%% -callback request_type() -> string().
%% -callback auth_info(#wm_reqdata{}, any()) -> {not_found | binary(), #wm_reqdata{}, any()}.

%% This is the max size allowed for incoming request bodies.
-define(MAX_SIZE, 1000000).

-include("chef_wm.hrl").

%% @doc Determines if service is available.
%%
%% Also initializes chef_db_context and reqid fields of base_state.
%% And handle other base_state init that depends on `Req'.
service_available(Req, State) ->
    %% TODO: query overload here and send 503 also can consult
    %% config/darklaunch to determine if we are in maint mode.
    OrgName = list_to_binary(wrq:path_info(organization_id, Req)),
    State0 = set_req_contexts(Req, State),
    State1 = State0#base_state{organization_name = OrgName},
    spawn_stats_hero_worker(Req, State1),
    {_GetHeader, State2} = chef_wm_util:get_header_fun(Req, State1),
    {true, Req, State2}.

forbidden(Req, #base_state{resource_mod = Mod} = State) ->
    case Mod:auth_info(Req, State) of
        {{halt, 403}, Req1, State1} ->
            {Req2, State2} = set_forbidden_msg(Req1, State1),
            {true, Req2, State2};
        {{halt, Code}, Req1, State1} ->
            {{halt, Code}, Req1, State1};
        {{create_in_container, Container}, Req1, State1} ->
            create_in_container(Container, Req1, State1);
        {{container, Container}, Req1, State1} ->
            ContainerId = fetch_container_id(Container, Req1, State1),
            invert_perm(check_permission(container, ContainerId, Req1, State1));
        {object, ObjectId, Req1, State1} ->
            invert_perm(check_permission(object, ObjectId, Req1, State1));
        {authorized, Req1, State1} ->
            {false, Req1, State1}
    end.

%% Called by forbidden/2 when the resource module wants to create a
%% new Chef Object within the container specified by the return value
%% of the resource module's auth_info function. We attempt to create
%% the authz object and return 403 if this fails due to lack of CREATE
%% permission. Otherwise, the created AuthzId is stored in the
%% resource_state record using set_authz_id/2 (which knows how to deal
%% with the different resource_state records).
create_in_container(Container, Req, #base_state{chef_authz_context = AuthzContext,
                                                organization_guid = OrgId,
                                                requestor = Requestor,
                                                resource_state = RS} = State) ->
    #chef_requestor{authz_id = RequestorId} = Requestor,
    case chef_authz:create_object_if_authorized(AuthzContext, OrgId, RequestorId,
                                                Container) of
        {ok, AuthzId} ->
            State1 = State#base_state{resource_state = set_authz_id(AuthzId, RS)},
            %% return forbidden: false
            {false, Req, State1};
        {error, forbidden} ->
            {Req1, State1} = set_forbidden_msg(Req, State),
            %% return forbidden: true
            {true, Req1, State1}
    end.

%% Called by forbidden/2 when the resource module wants to do authz based on the ACL of the
%% specified `Container'.
fetch_container_id(Container, _Req, #base_state{chef_authz_context = AuthzContext,
                                               organization_guid = OrgId}) ->
    chef_authz:get_container_aid_for_object(AuthzContext, OrgId, Container).

invert_perm({true, Req, State}) ->
    {false, Req, State};
invert_perm({false, Req, State}) ->
    {true, Req, State};
invert_perm(Other) ->
    Other.

check_permission(AuthzObjectType, AuthzId, Req, #base_state{reqid=ReqId,
                                                            requestor=Requestor}=State) ->
    #chef_requestor{authz_id = RequestorId} = Requestor,
    Perm = http_method_to_authz_perm(Req),
    case ?SH_TIME(ReqId, chef_authz, is_authorized_on_resource,
                  (RequestorId, AuthzObjectType, AuthzId, actor, RequestorId, Perm)) of
        true ->
            {true, Req, State};
        false ->
            {Req1, State1} = set_forbidden_msg(Req, State),
            {false, Req1, State1};
        Error ->
            error_logger:error_msg("is_authorized_on_resource failed (~p, ~p, ~p): ~p~n",
                                   [Perm, {AuthzObjectType, AuthzId}, RequestorId, Error]),
            {{halt, 500}, Req, State#base_state{log_msg={error, is_authorized_on_resource}}}
    end.


%% part of being authorized is being a member of the org; otherwise we
%% fail out early.
is_authorized(Req, State) ->
    case chef_wm_base:verify_request_signature(Req, State) of
        {true, Req1, State1} ->
            case authorized_by_org_membership_check(Req1,State1) of
                {false, Req2, State2} ->
                    {{halt, 403}, Req2, State2};
                {true, Req2, State2} ->
                    {true, Req2, State2}
            end;
        {false, ReqOther, StateOther} ->
            %% FIXME: the supported version is determined by the chef_authn application
            %% also, see: https://wiki.corp.opscode.com/display/CORP/RFC+Authentication+Version+Negotiation
            {"X-Ops-Sign version=\"1.0\" version=\"1.1\"", ReqOther, StateOther}
    end.

%% Clients are inherently a member of the org, but users are not.  If
%% we add a user to the org, and then disassociate them, there will be
%% acls left behind granting permissions on the org objects, so we
%% must check user association and permissions
authorized_by_org_membership_check(Req, #base_state{requester_type=client}=State) ->
    {true, Req, State};
authorized_by_org_membership_check(Req, State = #base_state{organization_name = OrgName,
                                                            chef_db_context = DbContext}) ->
    {UserName, BypassesChecks} = get_user(Req, State),
    case BypassesChecks of
        true -> {true, Req, State};
        _ ->
            case chef_db:is_user_in_org(DbContext, UserName, OrgName) of
                true ->
                    {true, Req, State};
                false ->
                    Msg = forbidden_message(not_member_of_org, UserName, OrgName),
                    {false, wrq:set_resp_body(ejson:encode(Msg), Req),
                     State#base_state{log_msg = user_not_in_org}};
                Error ->
                    Msg = forbidden_message(unverified_org_membership, UserName, OrgName),
                    {false, wrq:set_resp_body(ejson:encode(Msg), Req),
                     State#base_state{log_msg = {user_not_in_org_error, Error}}}
            end
    end.

set_forbidden_msg(Req, State) ->
    Perm = http_method_to_authz_perm(Req),
    Msg = iolist_to_binary(["missing ", atom_to_binary(Perm, utf8), " permission"]),
    JsonMsg = ejson:encode({[{<<"error">>, [Msg]}]}),
    Req1 = wrq:set_resp_body(JsonMsg, Req),
    {Req1, State#base_state{log_msg = {Perm, forbidden}}}.

forbidden_message(not_member_of_org, User, Org) ->
    Msg = iolist_to_binary([<<"'">>, User, <<"' not associated with organization '">>,
                            Org, <<"'">>]),
    {[{<<"error">>, [Msg]}]};
forbidden_message(unverified_org_membership, User, Org) ->
    Msg = iolist_to_binary([<<"Failed to verify user '">>, User,
                            <<"' as a member of organization '">>,
                            Org, <<"'">>]),
    {[{<<"error">>, [Msg]}]}.

-spec delete_object(chef_db:db_context(),
                    chef_object() | #chef_cookbook_version{},
                    object_id()) -> ok.
delete_object(DbContext, Object, RequestId) ->
    oc_chef_object_db:delete(DbContext, Object, RequestId).

set_req_contexts(Req, #base_state{reqid_header_name = HeaderName} = State) ->
    ReqId = read_req_id(HeaderName, Req),
    AuthzContext = chef_authz:make_context(ReqId),
    DbContext = chef_db:make_context(ReqId),
    State#base_state{chef_authz_context = AuthzContext,
                     chef_db_context = DbContext,
                     reqid = ReqId}.

read_req_id(ReqHeaderName, Req) ->
    case wrq:get_req_header(ReqHeaderName, Req) of
        undefined ->
            base64:encode(term_to_binary(make_ref()));
        HV ->
            iolist_to_binary(HV)
    end.

spawn_stats_hero_worker(Req, #base_state{resource_mod = Mod,
                                         organization_name = OrgName,
                                         reqid = ReqId}) ->
    RequestLabel = Mod:request_type(),
    Config = [{request_id, ReqId},
              {org_name, OrgName},
              %% FIXME: pull this out into app config
              {my_app, <<"chefAPI">>},
              {request_label, RequestLabel},
              {request_action, atom_to_list(wrq:method(Req))},
              %% FIXME: make this list a define/app config
              {upstream_prefixes, [<<"rdbms">>, <<"couch">>, <<"authz">>, <<"solr">>]}],
    stats_hero_worker_sup:new_worker(Config).

http_method_to_authz_perm(#wm_reqdata{}=Req) ->
    http_method_to_authz_perm(wrq:method(Req));
http_method_to_authz_perm('DELETE') ->
    delete;
http_method_to_authz_perm('GET') ->
    read;
http_method_to_authz_perm('POST') ->
    create;
http_method_to_authz_perm('PUT') ->
    update.

%% Tells whether this user is the superuser.
is_superuser(UserName) ->
    case application:get_env(oc_chef_wm, superusers) of
        {ok,Superusers} -> lists:member(UserName, Superusers);
        undefined -> false
    end.

%% Get the username from the request (and tell whether it is a superuser)
get_user(Req, #base_state{superuser_bypasses_checks = SuperuserBypassesChecks}) ->
    UserName = list_to_binary(wrq:get_req_header("x-ops-userid", Req)),
    BypassesChecks = SuperuserBypassesChecks andalso is_superuser(UserName),
    {UserName, BypassesChecks}.

set_authz_id(Id, #node_state{}=N) ->
    N#node_state{node_authz_id = Id};
set_authz_id(Id, #role_state{}=R) ->
    R#role_state{role_authz_id = Id};
set_authz_id(Id, #sandbox_state{}=S) ->
    S#sandbox_state{sandbox_authz_id = Id}.
