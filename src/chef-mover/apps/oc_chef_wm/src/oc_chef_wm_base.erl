%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith
%% @author Seth Falcon <seth@chef.io>
%% Copyright 2012-2014 Chef Software, Inc. All Rights Reserved.
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


-module(oc_chef_wm_base).

-include("oc_chef_wm.hrl").
-include_lib("public_key/include/public_key.hrl").

%% Complete webmachine callbacks
-export([content_types_accepted/2,
         content_types_provided/2,
         finish_request/2,
         forbidden/2,
         is_authorized/2,     %% verify request signature and org membership if appropriate
         malformed_request/2, %% verify request headers and size requirements, call module verify_request
         ping/2,
         post_is_create/2,
         service_available/2, %% initialize the request
         validate_request/3 %% default implementation always accepts request as valid
        ]).

%% "Grab Bag" common functionality - we may want to consider relocating these
%% since most aren't core operations of webmachine in chef server
-export([check_cookbook_authz/3,
         delete_object/3,
         object_creation_hook/2,
         object_creation_error_hook/2,
         stats_hero_label/1,
         stats_hero_upstreams/0,
         is_superuser/1,
         is_user_in_org/4,
         set_forbidden_msg/3,
         user_in_group/3]).

%% Helpers for webmachine callbacks
-export([create_from_json/5,
         init/2,
         list_objects_json/2, % Can also be used in lieu of to_json  in a resource module.
         update_from_json/4]).

%% @doc Determines if service is available.
%%
%% Also initializes chef_db_context and reqid fields of base_state.
%% And handle other base_state init that depends on `Req'.
service_available(Req, State) ->
    State0 = set_req_contexts(Req, State),
    State1 = maybe_with_default_org(Req, State0),
    spawn_stats_hero_worker(Req, State1),
    {_GetHeader, State2} = chef_wm_util:get_header_fun(Req, State1),
    {true, Req, State2}.

set_req_contexts(Req, #base_state{reqid_header_name = HeaderName} = State) ->
    ReqId = read_req_id(HeaderName, Req),
    {GetHeader, State1} = chef_wm_util:get_header_fun(Req, State),
    Darklaunch = xdarklaunch_req:parse_header(GetHeader),
    AuthzContext = oc_chef_authz:make_context(ReqId, Darklaunch),
    DbContext = chef_db:make_context(ReqId, Darklaunch),
    State1#base_state{chef_authz_context = AuthzContext,
                     chef_db_context = DbContext,
                     darklaunch = Darklaunch,
                     reqid = ReqId}.


content_types_accepted(Req, State) ->
    {[{"application/json", from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

maybe_with_default_org(Req, #base_state{organization_name = undefined} = State) ->
    State#base_state{organization_name = oc_chef_wm_routes:maybe_org_name(Req)};
maybe_with_default_org(_Req, #base_state{organization_name = OrgName} = State) when is_binary(OrgName) ->
    State.

forbidden(Req, #base_state{resource_mod = Mod} = State) ->
    case Mod:auth_info(Req, State) of
        {{halt, 403}, Req1, State1} ->
            {Req2, State2} = set_forbidden_msg(Req1, State1),
            {true, Req2, State2};
        {{halt, 403, Message}, Req1, State1} ->
            JsonMsg = chef_json:encode({[{<<"error">>, Message}]}),
            Req2 = wrq:set_resp_body(JsonMsg, Req1),
            State2 = State1#base_state{log_msg = {http_method_to_authz_perm(Req), forbidden}},
            {true, Req2, State2};
        {{halt, Code}, Req1, State1} ->
            {{halt, Code}, Req1, State1};
        {{create_in_container, Container}, Req1, State1} ->
            create_in_container(Container, Req1, State1);
        {{container, Container}, Req1, State1} ->
            ContainerId = fetch_container_id(Container, Req1, State1),
            invert_perm(check_permission(container, ContainerId, Req1, State1));
        {{container_id, AuthzId}, Req1, State1} ->
            invert_perm(check_permission(container, AuthzId, Req1, State1));
        {{group, AuthzId}, Req1, State1} ->
            invert_perm(check_permission(group, AuthzId, Req1, State1));
        {{group_id, AuthzId}, Req1, State1} ->
            invert_perm(check_permission(group, AuthzId, Req1, State1));
        {{Type, ObjectId, Permission}, Req1, State1} when Type =:= object;
                                                          Type =:= actor ->
            invert_perm(check_permission(Permission, Type, ObjectId, Req1, State1));
        {{Type, ObjectId}, Req1, State1} when Type =:= object;
                                              Type =:= actor ->
            invert_perm(check_permission(Type, ObjectId, Req1, State1));
        %% Several use cases around users and user auth are only
        %% available through the webui, eg superuser
        {superuser_only, Req1, State1} ->
            %% force superuser_bypasses_checks temporarily true so that get_user
            %% gives us an honest response as to whether the requestor is actually
            %% the superuser.
            case get_user(Req, State#base_state{superuser_bypasses_checks = true}) of
                {_, true} ->
                    {false, Req, State};
                _ ->
                    {Req2, State2} = set_forbidden_msg(Req1, State1),
                    {true, Req2, State2}
            end;
        {AuthTuples, Req1, State1} when is_list(AuthTuples)->
            %% NOTE: multi_auth_check does not handle create_in_container yet, and expects
            %% each auth tuple to have a permission.  This code path is currently only used
            %% by the depsolver endpoint.
            case multi_auth_check(AuthTuples, Req1, State1) of
                true ->
                    %% All auth checks out, so we're not forbidden
                    {false, Req1, State1};
                {false, {_AuthzObjectType, _AuthzId, Permission}} ->
                    %% NOTE: No specific message for the auth check that failed (but this is
                    %% the same behavior we had before)
                    {Req2, State2} = set_forbidden_msg(Permission, Req1, State1),
                    {true, Req2, State2};
                {Error, {AuthzObjectType, AuthzId, Permission}} ->
                    #base_state{requestor_id=RequestorId} = State1,
                    %% TODO: Extract this logging message, as it is used elsewhere, too
                    lager:error("is_authorized_on_resource failed (~p, ~p, ~p): ~p~n",
                                           [Permission, {AuthzObjectType, AuthzId}, RequestorId, Error]),
                    {{halt, 500}, Req, State1#base_state{log_msg={error, is_authorized_on_resource}}}
            end;
        {authorized, Req1, State1} ->
            {false, Req1, State1}
    end.

%% @doc Performs multiple authorization checks in sequence.  If all pass, returns true.  The
%% first check that is false or returns an error, however, halts short-circuits any further
%% checks and returns the result along with the auth_tuple() of the failing authorization
%% check (useful for error message generation).
-spec multi_auth_check(AuthChecks :: [auth_tuple()],
                       Req :: wm_req(),
                       State :: #base_state{}) -> true |
                                                  {false,
                                                   FailingTuple :: auth_tuple()} |
                                                  {Error :: term(),
                                                   FailingTuple :: auth_tuple()}.
multi_auth_check([], _Req, _State) ->
    %% Nothing left to check, must be OK
    true;
multi_auth_check([CurrentTuple|Rest], Req, State) ->
    case auth_check(CurrentTuple, Req, State) of
        true ->
            %% That one checked out; check the rest
            multi_auth_check(Rest, Req, State);
        false ->
            %% That one failed; no need to continue
            {false, CurrentTuple};
        Error ->
            %% That one REALLY failed; send it out for use in error messages
            {Error, CurrentTuple}
    end.

%% @doc Perform a simple authorization check.  Only indicates whether the requested
%% permission is allowed or not; does no manipulation of either Req or State.
%%
%% No function head for the `create_in_container` check, because that's not needed at this
%% time.  Further refactorings may change this.
%% -spec auth_check(AuthCheck :: auth_tuple(),
%%                  Req :: wm_req(),
%%                  State :: #base_state{}) -> true | false | Error :: term().
auth_check({container, Container, Permission}, Req, State) ->
    ContainerId = fetch_container_id(Container, Req, State),
    has_permission(container, ContainerId, Permission, Req, State);
auth_check({object, ObjectId, Permission}, Req, State) ->
    has_permission(object, ObjectId, Permission, Req, State);
auth_check({actor, ObjectId, Permission}, Req, State) ->
    has_permission(actor, ObjectId, Permission, Req, State).

%% Called by forbidden/2 when the resource module wants to create a
%% new Chef Object within the container specified by the return value
%% of the resource module's auth_info function. We attempt to create
%% the authz object and return 403 if this fails due to lack of CREATE
%% permission. Otherwise, the created AuthzId is stored in the
%% resource_state record using set_authz_id/2 (which knows how to deal
%% with the different resource_state records).
create_in_container(client=Container, Req,
                    #base_state{requestor=#chef_client{validator=true},
                                resource_state=#client_state{client_data=Data}
                               }=State) ->
    %% This function head is an abomination and an affront to all that is good and pure.
    %%
    %% HOWEVER, if we actually add validators to ACL of the client container, then they
    %% automatically get those same permissions on any subsequently-created clients, due to
    %% how we currently inherit the container ACL as a "template" for new items.
    %%
    %% It used to be the case that validators needed both CREATE and READ permission on
    %% the clients container (CREATE to actually create a client, READ to subsequently grab
    %% the ACL of the client container in order to merge it into the ACL of the new client).
    %% However, it appears that this requirement has been relaxed in recent history, such that
    %% READ is not explicitly required to read an ACL; membership in the ACL (any permission)
    %% is sufficient.
    %%
    %% This means that we only really need to handle the CREATE case. To do this, we
    %% effectively replace the validator's AuthzId with that of the Authz superuser (for this
    %% one operation!) and use it to do the creation.
    %%
    %% NOTE: having a CREATE permission on an already created object that isn't a container
    %% is, in fact, meaningless, but there's no sense in storing additional data.  We're
    %% going to loop back through later and fix up that situation.  Having extra "noise" in
    %% the ACLs is confusing anyway, and this is an area of our API where we should not be
    %% confusing.
    %
    %% TODO: we really should differentiate between "container permissions" and "permission
    %% templates".  We'll take a look at this in an upcoming version of Bifrost.
    %%
    %% Oh, and validators shouldn't be able to create other validators (to mirror the behavior
    %% of the Open Source Chef Server), so we need to check the contents of the
    %% client-to-be's data, which is a proplist, and so not very amenable to pattern
    %% matching :(
    CreatingAValidator = ej:get({<<"validator">>}, Data),

    case CreatingAValidator of
        true ->
            %% NOT IN MY HOUSE!
            {true, Req, State};  %% answers the question "is this operation forbidden?"
        false ->
            %% We'll pass the atom 'superuser' as a way to indicate to
            %% downstream code that this should be done by the Authz
            %% superuser (NOT the Chef API platform superuser!)

            do_create_in_container(Container, Req, State, superuser)
    end;
create_in_container(Container, Req, #base_state{requestor_id = RequestorId} = State) ->
    %% Here, the requestor isn't a validator client, so they should go through the normal
    %% auth checking process.
    do_create_in_container(Container, Req, State, RequestorId).

%% @doc Perform the actual creation of a new entity.
do_create_in_container(Container, Req,
                       #base_state{chef_authz_context = AuthzContext,
                                   organization_guid = OrgId,
                                   resource_state = RS} = State,
                       EffectiveRequestorId) ->
    case oc_chef_authz:create_entity_if_authorized(AuthzContext, OrgId,
                                                   EffectiveRequestorId, Container) of
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
%% TODO - Can we just dispense with the Req parameter since it isn't used??
fetch_container_id(Container, _Req, #base_state{chef_authz_context = AuthzContext,
                                                organization_guid = OrgId}) ->
    oc_chef_authz:get_container_aid_for_object(AuthzContext, OrgId, Container).

invert_perm({true, Req, State}) ->
    {false, Req, State};
invert_perm({false, Req, State}) ->
    {true, Req, State};
invert_perm(Other) ->
    Other.

%% @doc Performs simple permission check
%% -spec has_permission(AuthzObjectType :: authz_object(),
%%                      AuthzId :: object_id(),
%%                      Permission :: permission(),
%%                      Req :: wm_req(),
%%                      State :: #base_state{}) -> true | false | Error :: term().
has_permission(AuthzObjectType, AuthzId, Permission, _Req,
               #base_state{reqid=ReqId, requestor_id=RequestorId}) ->
    ?SH_TIME(ReqId, oc_chef_authz, is_authorized_on_resource,
             (RequestorId, AuthzObjectType, AuthzId, actor, RequestorId, Permission)).

%% NOTE: derives the permission check from the HTTP verb of the Request
check_permission(AuthzObjectType, AuthzId, Req, State) ->
    Perm = http_method_to_authz_perm(Req),
    check_permission(Perm, AuthzObjectType, AuthzId, Req, State).

check_permission(Perm, AuthzObjectType, AuthzId, Req, #base_state{requestor_id=RequestorId}=State) ->
    case has_permission(AuthzObjectType, AuthzId, Perm, Req, State) of
        true ->
            {true, Req, State};
        false ->
            {Req1, State1} = set_forbidden_msg(Req, State),
            {false, Req1, State1};
        Error ->
            lager:error("is_authorized_on_resource failed (~p, ~p, ~p): ~p~n",
                                   [Perm, {AuthzObjectType, AuthzId}, RequestorId, Error]),
            {{halt, 500}, Req, State#base_state{log_msg={error, is_authorized_on_resource}}}
    end.

%% part of being authorized is being a member of the org; otherwise we
%% fail out early.
is_authorized(Req, State) ->
    case verify_request_signature(Req, State) of
        {true, Req1, State1} ->
            case authorized_by_org_membership_check(Req1,State1) of
                {false, Req2, State2} ->
                    {{halt, 403}, Req2, State2};
                {true, Req2, State2} ->
                    {true, Req2, State2}
            end;
        {false, ReqOther, StateOther} ->
            %% FIXME: the supported version is determined by the chef_authn application
            %% also, see: https://wiki.corp.chef.io/display/CORP/RFC+Authentication+Version+Negotiation
            {"X-Ops-Sign version=\"1.0\" version=\"1.1\"", ReqOther, StateOther}
    end.

%% Clients are inherently a member of the org, but users are not.  For purposes of acl checks,
%% requests that have no org (/users, /verify_password, etc) will be considered automatically valid/
%% part of the (nonexistant) org, leaving it up to checks of individual resources to make
%% further determinations.
%%
%% If we add a user to the org, and then disassociate them, there will be
%% acls left behind granting permissions on the org objects, so we
%% must check user association and permissions
authorized_by_org_membership_check(Req, #base_state{requestor=#chef_client{}}=State) ->
    {true, Req, State};
authorized_by_org_membership_check(Req, #base_state{organization_name = undefined} = State) ->
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
                    {false, wrq:set_resp_body(chef_json:encode(Msg), Req),
                     State#base_state{log_msg = user_not_in_org}};
                Error ->
                    Msg = forbidden_message(unverified_org_membership, UserName, OrgName),
                    {false, wrq:set_resp_body(chef_json:encode(Msg), Req),
                     State#base_state{log_msg = {user_not_in_org_error, Error}}}
            end
    end.


set_forbidden_msg(Perm, Req, State) when is_atom(Perm)->
    Msg = iolist_to_binary(["missing ", atom_to_binary(Perm, utf8), " permission"]),
    JsonMsg = chef_json:encode({[{<<"error">>, [Msg]}]}),
    Req1 = wrq:set_resp_body(JsonMsg, Req),
    {Req1, State#base_state{log_msg = {Perm, forbidden}}}.

%% Assumes the permission can be derived from the HTTP verb of the request; this is the
%% original behavior of this function, prior to the addition of set_forbidden_msg/3.
set_forbidden_msg(Req, State) ->
    Perm = http_method_to_authz_perm(Req),
    set_forbidden_msg(Perm, Req, State).

forbidden_message(not_member_of_org, User, Org) ->
    Msg = iolist_to_binary([<<"'">>, User, <<"' not associated with organization '">>, Org, <<"'">>]),
    {[{<<"error">>, [Msg]}]};
forbidden_message(unverified_org_membership, User, Org) ->
    Msg = iolist_to_binary([<<"Failed to verify user '">>, User, <<"' as a member of organization '">>, Org, <<"'">>]),
    {[{<<"error">>, [Msg]}]}.

-spec delete_object(chef_db:db_context(),
                    chef_object() |
                    #chef_cookbook_version{} |
                    #oc_chef_container{} |
                    #oc_chef_group{} |
                    #oc_chef_org_user_association{} |
                    #oc_chef_org_user_invite{},
                    object_id()) -> ok.
delete_object(DbContext, Object, RequestorId) ->
    oc_chef_object_db:delete(DbContext, Object, RequestorId).

read_req_id(ReqHeaderName, Req) ->
    case wrq:get_req_header(ReqHeaderName, Req) of
        undefined ->
            base64:encode(term_to_binary(make_ref()));
        HV ->
            iolist_to_binary(HV)
    end.

spawn_stats_hero_worker(Req, #base_state{resource_mod = Mod,
                                         organization_name = OrgName,
                                         reqid = ReqId,
                                         metrics_config = MetricsConfig}) ->
    RequestLabel = Mod:request_type(),
    Config = [{request_id, ReqId},
              {org_name, OrgName},
              {my_app, ?gv(root_metric_key, MetricsConfig)},
              {request_label, RequestLabel},
              {request_action, atom_to_list(wrq:method(Req))},
              {label_fun, ?gv(stats_hero_label_fun, MetricsConfig)},
              {upstream_prefixes, ?gv(stats_hero_upstreams, MetricsConfig)}],
    %% we don't want to fail if stats_hero is broken, but will log an error message if we
    %% can't even spawn a worker here.
    case stats_hero_worker_sup:new_worker(Config) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            lager:error("FAILED stats_hero_worker_sup:new_worker: ~p~n",
                                   [Reason]),
            ok
    end.

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
    Superusers = envy:get(oc_chef_wm, superusers, [], list),
    lists:member(UserName, Superusers).

%% Get the username from the request (and tell whether it is a superuser)
get_user(Req, #base_state{superuser_bypasses_checks = SuperuserBypassesChecks}) ->
    UserName = list_to_binary(wrq:get_req_header("x-ops-userid", Req)),
    BypassesChecks = SuperuserBypassesChecks andalso is_superuser(UserName),
    {UserName, BypassesChecks}.

set_authz_id(Id, #client_state{}=Cl) ->
    Cl#client_state{client_authz_id = Id};
set_authz_id(Id, #cookbook_state{}=C) ->
    C#cookbook_state{authz_id = Id};
set_authz_id(Id, #environment_state{}=E) ->
    E#environment_state{environment_authz_id = Id};
set_authz_id(Id, #node_state{}=N) ->
    N#node_state{node_authz_id = Id};
set_authz_id(Id, #role_state{}=R) ->
    R#role_state{role_authz_id = Id};
set_authz_id(Id, #sandbox_state{}=S) ->
    S#sandbox_state{sandbox_authz_id = Id};
set_authz_id(Id, #data_state{}=D) ->
    D#data_state{data_bag_authz_id = Id};
set_authz_id(Id, #container_state{} = C) ->
    C#container_state{container_authz_id = Id};
set_authz_id(Id, #group_state{} = G) ->
    G#group_state{group_authz_id = Id};
set_authz_id(Id, #organization_state{} = O) ->
    O#organization_state{organization_authz_id = Id};
set_authz_id(Id, #user_state{} = U) ->
    U#user_state{user_authz_id = Id}.



%%------------------------------------------------------------------------------
%% GRAB BAG FUNCTIONS AHEAD!!
%%------------------------------------------------------------------------------
%%
%% The following functions require the use of Authz, but in ways that are not currently
%% amenable to our behaviour / mixin based approach.  The most expedient thing at present is
%% to export these functions and call them directly via oc_chef_wm in the endpoints
%% where they are required.
%%
%% As such, the Open Source implementation of the base resource will need corresponding
%% "no-op" versions.

%% @doc Check the READ authz permissions on a list of cookbooks in parallel.  Checks ALL
%% cookbooks to return a complete error message, since if the user had read permission on
%% all cookbooks, we'd be making all the HTTP requests anyway.
-spec check_cookbook_authz(Cookbooks :: [#chef_cookbook_version{}],
                           Req :: wm_req(),
                           State :: #base_state{}) ->
                                  ok | {error, {[any(),...]}}.
check_cookbook_authz(Cookbooks, _Req, #base_state{reqid = ReqId,
                                                        requestor_id = RequestorId}) ->
    Resources = [{AuthzId, Name}
                 || #chef_cookbook_version{name = Name, authz_id = AuthzId} <- Cookbooks],
    case ?SH_TIME(ReqId, oc_chef_authz, bulk_actor_is_authorized, (ReqId, RequestorId, object, Resources, read)) of
        true -> ok;
        {error, Why} ->
            Report = {check_cookbook_authz, {Why, ReqId}},
            lager:error("~p", [Report]),
            error(Report);
        {false, NoAuthzList} ->
            {error, {[{<<"message">>, <<"Read permission is not granted for one or more cookbooks">>},
                      {<<"unauthorized_cookbooks">>, NoAuthzList}]}}
    end.

%% This version should work for Open Source:
%% check_cookbook_authz(_Cookbooks, _Req, _State) -> ok.

is_user_in_org(Type, DbContext, Name, OrgName) ->
    case Type of
        <<"client">> ->
            true;
        <<"user">> ->
            case chef_db:is_user_in_org(DbContext, Name, OrgName) of
                true ->
                    true;
                false ->
                    false;
                Error ->
                    throw(Error)
            end
    end.


%% These are modules that we instrument with stats_hero and aggregate into common prefix via
%% stats_hero_label.
-type metric_module() :: oc_chef_authz | chef_s3 | chef_sql | chef_solr | chef_otto.

%% @doc Given a `{Mod, Fun}' tuple, generate a stats hero metric with a prefix appropriate
%% for stats_hero aggregation. An error is thrown if `Mod' is unknown. This is where we
%% encode the mapping of module to upstream label.
-spec stats_hero_label({Mod::metric_module(), Fun::atom()}) -> <<_:16,_:_*8>>.
stats_hero_label({chef_sql, Fun}) ->
    chef_metrics:label(rdbms, {chef_sql, Fun});
stats_hero_label({oc_chef_authz, Fun}) ->
    chef_metrics:label(authz, {oc_chef_authz, Fun});
stats_hero_label({chef_solr, Fun}) ->
    chef_metrics:label(solr, {chef_solr, Fun});
stats_hero_label({chef_s3, Fun}) ->
    chef_metrics:label(s3, {chef_s3, Fun});
stats_hero_label({chef_depsolver, Fun}) ->
    chef_metrics:label(depsolver, {chef_depsolver, Fun});
stats_hero_label({BadPrefix, Fun}) ->
    erlang:error({bad_prefix, {BadPrefix, Fun}}).


%% @doc The prefixes that stats_hero should use for aggregating timing data over each
%% request.
stats_hero_upstreams() ->
    [<<"authz">>, <<"depsolver">>, <<"rdbms">>, <<"s3">>, <<"solr">>].



object_creation_hook(#chef_client{}=Client,
                     #base_state{chef_authz_context=AuthContext,
                                 organization_guid = OrgId}) ->
    %% NOTE: client create authorization is handled entirely by having CREATE on the clients
    %% container. We use system privilege to add the client to the clients group without
    %% regard for the ACL on the clients group.
    client_cleanup(Client, AuthContext, OrgId, superuser);
object_creation_hook(Object, _State) ->
    %% Everything else passes through unaffected
    Object.


%% @doc Perform needed post-creation cleanup on Client objects.
%% Clients must be added to the clients group, and newly-created
%% validators must have themselves removed from their ACL.
%%
%% See oc_chef_authz:add_client_to_clients_group/4 for more
%% information on the `RequestorId` argument
-spec client_cleanup(#chef_client{},
                     AuthContext :: chef_authz:chef_authz_context(),
                     OrgId :: object_id(),
                     RequestorId :: superuser) -> #chef_client{} |
                                                                {error, term()}.
client_cleanup(#chef_client{authz_id=ClientAuthzId,
                            validator=IsValidator}=Client,
               AuthContext,
               OrgId,
               RequestorId) ->
    case oc_chef_authz:add_client_to_clients_group(AuthContext, OrgId, ClientAuthzId, RequestorId) of
        ok ->
            case IsValidator of
                true ->
                    %% Validators have no permissions on anything; remove it from its own ACL
                    oc_chef_authz:remove_actor_from_actor_acl(ClientAuthzId, ClientAuthzId);
                false ->
                    ok %% No need to remove anything otherwise
            end,

            %% Return the client
            Client;
        {error, Error} ->
            {error, Error}
    end.

object_creation_error_hook(#chef_data_bag_item{}, _RequestorId) ->
    ok;
object_creation_error_hook(#chef_cookbook_version{}, _RequestorId) ->
    ok;
object_creation_error_hook(Object, RequestorId) ->
    oc_chef_authz:delete_resource(RequestorId, object, chef_object:authz_id(Object)),
    ok.

%% Evaluates if user is a direct member of a group
user_in_group(#base_state{organization_guid = OrgId, chef_db_context = DbContext},
              UserName, GroupName) ->
    case chef_db:fetch(#oc_chef_group{org_id = OrgId, for_requestor_id = oc_chef_authz:superuser_id(), name = GroupName}, DbContext) of
        #oc_chef_group{users = Users} ->
            lists:member(UserName, Users);
        not_found ->
            not_found
    end.

finish_request(Req, #base_state{reqid = ReqId,
                                organization_name = OrgName,
                                darklaunch = Darklaunch}=State) ->
    try
        Code = wrq:response_code(Req),
        PerfTuples = stats_hero:snapshot(ReqId, agg),
        UserId = wrq:get_req_header("x-ops-userid", Req),
        Req0 = oc_wm_request:add_notes([{req_id, ReqId},
                                        {user, UserId},
                                        {perf_stats, PerfTuples}], Req),
        Req1 = maybe_annotate_log_msg(Req0, State),
        AnnotatedReq = maybe_annotate_org_specific(OrgName, Darklaunch, Req1),
        stats_hero:report_metrics(ReqId, Code),
        stats_hero:stop_worker(ReqId),
        log_action(Req, State),
        case Code of
            500 ->
                % Sanitize response body
                ErrReq = create_500_response(AnnotatedReq, State),
                {true, ErrReq, State};
            _ ->
                 AnnotatedReq1 = add_api_info_header(AnnotatedReq, State),
                 {true, AnnotatedReq1, State}
        end
    catch
        X:Y:Stacktrace ->
            lager:error({X, Y, Stacktrace})
    end;
finish_request(_Req, Anything) ->
    lager:error("chef_wm:finish_request/2 did not receive #base_state{}~nGot: ~p~n", [Anything]).

log_action(Req, State)->
    Action = envy:get(oc_chef_wm, enable_actions, false, boolean),
    maybe_log_action(Action, Req, State).

maybe_log_action(true, Req, State) ->
    oc_chef_action:log_action(Req, State);
maybe_log_action(false, _Req, _State) ->
    ok.

init(ResourceMod, Config) ->
    BaseState = init_base_state(ResourceMod, Config),
    case ResourceMod:init_resource_state(Config) of
        {ok, ResourceState} ->
            maybe_trace(BaseState#base_state{resource_state=ResourceState}, Config);
        Error ->
            Error
    end.

ping(Req, State) ->
    {pong, Req, State}.

init_base_state(ResourceMod, InitParams) ->
    #base_state{reqid_header_name = ?gv(reqid_header_name, InitParams),
                auth_skew = ?gv(auth_skew, InitParams),

                %% default orgname support
                organization_name = ?gv(organization_name, InitParams),

                otp_info = ?gv(otp_info, InitParams),
                server_flavor = ?gv(server_flavor, InitParams),
                api_version = ?gv(api_version, InitParams),

                metrics_config = ?gv(metrics_config, InitParams),

                resource_args = ?gv(resource_args, InitParams),
                resource_mod = ResourceMod}.

validate_request(_Verb, Req, State) ->
    {Req, State}.

post_is_create(Req, State) ->
    {true, Req, State}.

malformed_request(Req, #base_state{resource_mod=Mod,
                                   auth_skew=AuthSkew}=State) ->
    {GetHeader, State1} = chef_wm_util:get_header_fun(Req, State),
    try
        chef_authn:validate_headers(GetHeader, AuthSkew),
        Req1 = chef_wm_enforce:max_size(Req),
        {OrgId, OrgAuthzId} = chef_wm_util:fetch_org_metadata(State1),
        {Req2, State2} = Mod:validate_request(wrq:method(Req1), Req1,
                                              State1#base_state{organization_guid = OrgId,
                                                                organization_authz_id = OrgAuthzId}),
        {false, Req2, State2}
    catch
        throw:{org_not_found, Org} ->
            Msg = iolist_to_binary([<<"organization '">>, Org, <<"' does not exist.">>]),
            Req3 = wrq:set_resp_body(chef_json:encode({[{<<"error">>, [Msg]}]}), Req),
            {{halt, 404}, Req3, State1#base_state{log_msg = org_not_found}};
        throw:bad_clock ->
            Msg1 = chef_wm_malformed:malformed_request_message(bad_clock, Req, State),
            Req3 = wrq:set_resp_body(chef_json:encode(Msg1), Req),
            {{halt, 401}, Req3, State1#base_state{log_msg = bad_clock}};
        throw:{bad_headers, Headers} ->
            Msg1 =  chef_wm_malformed:malformed_request_message({bad_headers, Headers}, Req, State),
            Req3 = wrq:set_resp_body(chef_json:encode(Msg1), Req),
            {{halt, 401}, Req3, State1#base_state{log_msg = bad_headers}};
        throw:bad_sign_desc ->
            Msg1 =  chef_wm_malformed:malformed_request_message(bad_sign_desc, Req, State),
            Req3 = wrq:set_resp_body(chef_json:encode(Msg1), Req),
            {{halt, 400}, Req3, State1#base_state{log_msg = bad_sign_desc}};
        throw:{too_big, Msg} ->
            lager:info("json too large (~p)", [Msg]),
            Req3 = wrq:set_resp_body(chef_json:encode({[{<<"error">>, Msg}]}), Req),
            {{halt, 413}, Req3, State1#base_state{log_msg = too_big}};
        throw:Why ->
            Msg =  chef_wm_malformed:malformed_request_message(Why, Req, State),
            NewReq = wrq:set_resp_body(chef_json:encode(Msg), Req),
            {true, NewReq, State1#base_state{log_msg = Why}}
    end.


create_500_response(Req, State) ->
    %% sanitize response body
    Msg = <<"internal service error">>,
    Json = chef_json:encode({[{<<"error">>, [Msg]}]}),
    Req1 = wrq:set_resp_header("Content-Type",
                               "application/json", Req),
    Req2 = add_api_info_header(Req1, State),
    wrq:set_resp_body(Json, Req2).

%% @doc Extract information from `State' needed to generate the X-Ops-API-Info header value.
api_info(#base_state{api_version = ApiVersion,
                     otp_info = {ReleaseName, OtpVersion},
                     server_flavor = ServerFlavor}) ->
    [{"flavor", ServerFlavor},
     {"version", ApiVersion},
     {ReleaseName, OtpVersion}].

%% @doc Generate the value of the X-Ops-API-Info header, which is a semicolon-delimited list
%% of key=value pairs.
api_info_header_value(#base_state{}=State) ->
    string:join([ Key ++ "=" ++ Value ||
                    {Key, Value} <- api_info(State)],
                ";").

%% @doc Add the X-Ops-API-Info header to the outgoing response.  This contains server API
%% version information (useful for maintaining back-compatibility) as well as OTP version
%% information (more useful for debugging purposes).
add_api_info_header(Req, State) ->
    wrq:set_resp_header("X-Ops-API-Info", api_info_header_value(State), Req).

-spec verify_request_signature(#wm_reqdata{}, #base_state{}) ->
                                      {boolean(), #wm_reqdata{}, #base_state{}}.
%% @doc Perform request signature verification (authenticate)
%%
%% Fetches user or client certificate and uses it verify the signature
%% on the request.  If the request cannot be verified, then the
%% returned `#wm_reqdata{}' record will have a response body
%% explaining why.
verify_request_signature(Req,
                         #base_state{organization_name = OrgName,
                                     organization_guid = OrgId,
                                     auth_skew = AuthSkew,
                                     chef_db_context = DbContext}=State) ->
    UserName = wrq:get_req_header("x-ops-userid", Req),
    case chef_db:fetch_requestor(DbContext, OrgId, UserName) of
        {not_found, What} ->
            NotFoundMsg = verify_request_message({not_found, What},
                                                 UserName, OrgName),
            {false, wrq:set_resp_body(chef_json:encode(NotFoundMsg), Req),
             State#base_state{log_msg = {not_found, What}}};
        Requestor -> %% This is either #chef_client{} or #chef_user{}
            %% If the request originated from the webui, we do authn using the webui public
            %% key, not the user's key.
            PublicKey = select_user_or_webui_key(Req, Requestor),
            Body = body_or_default(Req, <<>>),
            HTTPMethod = method_as_binary(Req),
            Path = iolist_to_binary(wrq:path(Req)),
            {GetHeader, State1} = chef_wm_util:get_header_fun(Req, State),
            case chef_authn:authenticate_user_request(GetHeader, HTTPMethod,
                                                      Path, Body, PublicKey,
                                                      AuthSkew) of
                {name, _} ->
                    {true, Req, State1#base_state{requestor_id = authz_id(Requestor),
                                                  requestor = Requestor}};
                {no_authn, Reason} ->
                    Msg = verify_request_message(Reason, UserName, OrgName),
                    Json = chef_json:encode(Msg),
                    Req1 = wrq:set_resp_body(Json, Req),
                    {false, Req1, State1#base_state{log_msg = Reason}}
            end
    end.

-spec create_from_json(Req :: #wm_reqdata{}, State :: #base_state{},
                       RecType :: chef_object_name()| chef_cookbook_version,
                       ContainerId ::object_id() | {authz_id, AuthzId::object_id()},
                       ObjectEjson :: ejson_term()) ->
                              {true | {halt, 409 | 500}, #wm_reqdata{}, #base_state{}}.
%% @doc Implements the from_json callback for POST requests to create Chef
%% objects. `RecType' is the name of the object record being created
%% (e.g. `chef_node'). `ContainerId' is the AuthzID of the container for the object being
%% created (e.g. node container authz ID for creating a node). The `ObjectEjson' is the
%% validated and normalized EJSON that was parsed from the request body.
create_from_json(#wm_reqdata{} = Req, #base_state{organization_guid = undefined} = State,
                                                  RecType, AuthzData, ObjectEJson) ->
    % For objects that are not a member of an org, we just need to provide a valid ID
    % for guid generation.
    create_from_json(Req, State#base_state{organization_guid = ?OSC_ORG_ID},
                     RecType, AuthzData, ObjectEJson);
create_from_json(#wm_reqdata{} = Req,
                 #base_state{chef_db_context = DbContext,
                             organization_guid = OrgId,
                             requestor_id = ActorId,
                             resource_mod = ResourceMod} = State,
                 RecType, {authz_id, AuthzId}, ObjectEjson) ->
    %% ObjectEjson should already be normalized. Record creation does minimal work and does
    %% not add or update any fields.
    ObjectRec = chef_object:new_record(RecType, OrgId, maybe_authz_id(AuthzId), ObjectEjson),
    Name = chef_object:name(ObjectRec),
    TypeName = chef_object:type_name(ObjectRec),

    %% Perform any additional platform-specific work on the object
    ObjectRec = object_creation_hook(ObjectRec, State),

    %% We send the object data to solr for indexing *first*. If it fails, we'll error out on
    %% a 500 and client can retry. If we succeed and the db call fails or conflicts, we can
    %% safely send a delete to solr since this is a new object with a unique ID unknown to
    %% the world.
    ok = oc_chef_object_db:add_to_solr(ObjectRec, ObjectEjson),
    case chef_db:create(ObjectRec, DbContext, ActorId) of
        {conflict, _} ->
            %% ignore return value of solr delete, this is best effort.
            oc_chef_object_db:delete_from_solr(ObjectRec),
            object_creation_error_hook(ObjectRec, ActorId),
            %% FIXME: created authz_id is leaked for this case, cleanup?
            LogMsg = {RecType, name_conflict, Name},
            ConflictMsg = ResourceMod:conflict_message(Name),
            {{halt, 409}, chef_wm_util:set_json_body(Req, ConflictMsg),
             State#base_state{log_msg = LogMsg}};
        ok ->
            LogMsg = {created, Name},
            Uri = oc_chef_wm_routes:route(TypeName, Req, [{name, Name}]),
            {true,
             chef_wm_util:set_uri_of_created_resource(Uri, Req),
             State#base_state{log_msg = LogMsg}};
        What ->
            %% ignore return value of solr delete, this is best effort.
            %% FIXME: created authz_id is leaked for this case, cleanup?
            oc_chef_object_db:delete_from_solr(ObjectRec),
            object_creation_error_hook(ObjectRec, ActorId),
            {{halt, 500}, Req, State#base_state{log_msg = What}}
    end.

-spec update_from_json(#wm_reqdata{},
                       #base_state{},
                       chef_updatable_object() | #chef_user{},
                       ejson_term()) ->
                              {true, #wm_reqdata{}, #base_state{}} |
                              {{halt, 400 | 404 | 500}, #wm_reqdata{}, #base_state{}}.
%% @doc Implements the from_json callback for PUT requests to update Chef
%% objects. `OrigObjectRec' should be the existing and unmodified `chef_object()'
%% record. `ObjectEjson' is the parsed EJSON from the request body.
update_from_json(#wm_reqdata{} = Req, #base_state{chef_db_context = DbContext,
                                                  requestor_id = ActorId,
                                                  resource_mod = ResourceMod} = State,
                 OrigObjectRec, ObjectEjson) ->
    ObjectRec = chef_object:update_from_ejson(OrigObjectRec, ObjectEjson),

    %% Send object to solr for indexing *first*. If the update fails, we will have sent
    %% incorrect data, but that should get corrected when the client retries. This is a
    %% compromise.
    ok = oc_chef_object_db:add_to_solr(ObjectRec, ObjectEjson),

    %% Ignore updates that don't change anything. If the user PUTs identical data, we skip
    %% going to the database and skip updating updated_at. This allows us to avoid RDBMS
    %% specific behavior around updates with unchanged data and race conditions around
    %% updated_at having resolution only to seconds. It also allows us treat updated_at as
    %% an indicator of when the data actually changed.
    case OrigObjectRec =:= ObjectRec of
        true ->
            State1 = State#base_state{log_msg = ignore_update_for_duplicate},
            {true, chef_wm_util:set_json_body(Req, ObjectEjson), State1};
        false ->
            case chef_db:update(ObjectRec, DbContext, ActorId) of
                ok ->
                    IsRename = chef_object:name(OrigObjectRec) =/= chef_object:name(ObjectRec),
                    Req1 = handle_rename(ObjectRec, Req, IsRename),
                    {true, chef_wm_util:set_json_body(Req1, ObjectEjson), State};
                not_found ->
                    %% We will get this if no rows were affected by the query. This could
                    %% happen if the object is deleted in the middle of handling this
                    %% request. In this case, we return 404 just as we would if the client
                    %% retried.
                    State1 = State#base_state{log_msg = not_found},
                    Msg = chef_wm_util:not_found_message(chef_object:type_name(ObjectRec),
                                                           chef_object:name(ObjectRec)),
                    Req1 = chef_wm_util:set_json_body(Req, Msg),
                    {{halt, 404}, Req1, State1};
                {conflict, _} ->
                    Name = chef_object:name(ObjectRec),
                    RecType = erlang:element(1,ObjectRec),
                    LogMsg = {RecType, name_conflict, Name},
                    ConflictMsg = ResourceMod:conflict_message(Name),
                    {{halt, 409}, chef_wm_util:set_json_body(Req, ConflictMsg),
                     State#base_state{log_msg = LogMsg}};
                {error, {checksum_missing, Checksum}} ->
                    %% Catches the condition where the user attempts to reference a checksum that
                    %% as not been uploaded.
                    %% This leaves it open to be generified
                    %% Not sure if we want to explicitly assume what is getting passed
                    %% is chef_cookbook_version
                    LogMsg = {checksum_missing, Checksum},
                    ErrorMsg = error_message(checksum_missing, Checksum),
                    {{halt, 400}, chef_wm_util:set_json_body(Req, ErrorMsg),
                     State#base_state{log_msg = LogMsg}};
                Why ->
                    State1 = State#base_state{log_msg = Why},
                    {{halt, 500}, Req, State1}
            end
    end.

error_message(checksum_missing, Checksum) ->
    {[{<<"error">>, [iolist_to_binary([<<"Manifest has checksum ">>, Checksum,
                                       <<" but it hasn't yet been uploaded">>])]}]}.

verify_request_message({not_found, org}, _User, Org) ->
    Msg = iolist_to_binary([<<"organization '">>, Org, <<"' does not exist.">>]),
    {[{<<"error">>, [Msg]}]};
verify_request_message({not_found, _}, User, _Org) ->
    Msg = iolist_to_binary([<<"Failed to authenticate as '">>, User, <<"'. ">>,
                            <<"Ensure that your node_name and client key ">>,
                            <<"are correct.">>]),
    {[{<<"error">>, [Msg]}]};
verify_request_message(bad_sig, User, _Org) ->
    Msg = iolist_to_binary([<<"Invalid signature for user or client '">>,
                            User,<<"'">>]),
    {[{<<"error">>, [Msg]}]};
verify_request_message(ErrorType, User, Org)  when
      ErrorType =:= not_associated_with_org orelse
      ErrorType =:= unverified_org_membership ->
    Msg = iolist_to_binary([<<"'">>, User,
                            <<"' is not associated with organization '">>,
                            Org, <<"'">>]),
    {[{<<"error">>, [Msg]}]}.

body_or_default(Req, Default) ->
    case wrq:req_body(Req) of
        undefined -> Default;
        Body -> Body
    end.


maybe_annotate_org_specific(?OSC_ORG_NAME, _Darklaunch, Req) ->
    Req;
maybe_annotate_org_specific(OrgName, Darklaunch, Req) ->
    %% Generate the darklaunch header in a form that won't break log parsing
    DLData = chef_wm_darklaunch:get_proplist(Darklaunch),
    oc_wm_request:add_notes([{org_name, OrgName},
                             {darklaunch, DLData}], Req).

%% Filters out log_msg that are undefined
maybe_annotate_log_msg(Req, #base_state{log_msg = undefined}) ->
    Req;
maybe_annotate_log_msg(Req, #base_state{log_msg = Msg}) ->
    oc_wm_request:add_notes([{msg, {raw, Msg}}], Req).

%% If request results in a rename, then set Location header and wm will return with a 201.
%% When we rename an object, we want to return 201 because the location of the object
%% has now changed. Setting the location header here to correspond to the new name
%% will force webmachine to return 201.
handle_rename(_ObjectRec, Req, false) ->
    Req;
handle_rename(ObjectRec, Req, true) ->
    TypeName = chef_object:type_name(ObjectRec),
    ObjectName = case chef_object:name(ObjectRec) of
                     {_ParentName, ObjName} -> ObjName; % ugh, special case for databag items
                     ObjName -> ObjName
                 end,
    Uri = oc_chef_wm_routes:route(TypeName, Req, [{name, ObjectName}]),
    wrq:set_resp_header("Location", binary_to_list(Uri), Req).

%%% @doc Return appropriate public key based on request source
%%%
%%% Requests coming from the webui, marked by the 'X-Ops-Request-Source' header read the
%%% webui public key and use that for authn. Otherwise this function just passes through the
%%% "KeyData" arg which is the user or client public key.
%%%
%%% The webui public key is fetched from the chef_keyring service. The 'X-Ops-WebKey-Tag'
%%% header specifies which key id to use, or we use the 'default' key if it is missing.
%%%
select_user_or_webui_key(Req, Requestor) ->
    %% Request origin is determined by the X-Ops-Request-Source header.  This is still secure
    %% because the request needs to have been signed with the webui private key.
    case wrq:get_req_header("x-ops-request-source", Req) of
        "web" ->
            WebKeyTag =
                case wrq:get_req_header("x-ops-webkey-tag", Req) of
                    undefined ->
                        default;
                    "" ->
                        default;
                    Tag ->
                        try
                            list_to_existing_atom(Tag)
                        catch
                            %% The proplist for webui_pub_key_list has been parsed, so the
                            %% key should exist as an atom
                            throw:badarg:Stacktrace ->
                                lager:error({"unknown webkey tag", Tag,
                                                           Stacktrace}),
                                %% alternately, we could just use the default key instead of failing;
                                %% but I prefer noisy errors
                                throw({badarg, "unknown webkey tag", Tag})
                        end
                end,
            case chef_keyring:get_key(WebKeyTag) of
                %% extract the public key from the private key
                {ok, #'RSAPrivateKey'{modulus=Mod, publicExponent=Exp}} ->
                    #'RSAPublicKey'{modulus = Mod, publicExponent = Exp};
                {ok, #'RSAPublicKey'{}=PublicKey} ->
                    PublicKey;
                {error, unknown_key} ->
                    Msg = io_lib:format("Failed finding key ~w", [WebKeyTag]),
                    lager:error({no_such_key, Msg, [?MODULE, ?LINE]}),
                    throw({no_such_key, WebKeyTag})
            end;
        _Else ->
            public_key(Requestor)
    end.

method_as_binary(Req) ->
    iolist_to_binary(atom_to_list(wrq:method(Req))).

maybe_trace(State, Config) ->
    case lists:keyfind(trace, 1, Config) of
        {trace, true} ->
            {{trace, "/tmp"}, State};
        _ ->
            {ok, State}
    end.

maybe_authz_id(undefined) ->
    unset;
maybe_authz_id(B) ->
    B.


-spec authz_id(#chef_user{} | #chef_client{}) -> object_id().
authz_id(#chef_client{authz_id = AuthzId}) ->
    AuthzId;
authz_id(#chef_user{authz_id = AuthzId}) ->
    AuthzId.

-spec public_key(#chef_user{} | #chef_client{}) -> binary().
public_key(#chef_user{public_key = PublicKey}) ->
    PublicKey;
public_key(#chef_client{public_key = PublicKey}) ->
    PublicKey.


%% @doc Webmachine content producing callback (that can be wired into
%% content_types_provided) that returns a JSON map of object names to object URLs. This
%% leverages the `chef_object' behavior and relies upon a stub object record with `org_id'
%% being present in `State#base_state.resource_state'.
%%
%% Note that since this module provides {@link content_types_provided/2} with a hard-coded
%% callback of `to_json', you can make use of this function using mixer and renaming it to
%% to_json.
-spec list_objects_json(#wm_reqdata{}, #base_state{}) -> {binary(), #wm_reqdata{}, #base_state{}}.
list_objects_json(Req, #base_state{chef_db_context = DbContext,
                                   resource_state = StubRec} = State) ->
    Names = chef_db:list(StubRec, DbContext),
    RouteFun = oc_chef_wm_routes:bulk_route_fun(chef_object:type_name(StubRec), Req),
    UriMap= [{Name, RouteFun(Name)} || Name <- Names],
    {chef_json:encode({UriMap}), Req, State}.



