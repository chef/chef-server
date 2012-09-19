%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @author Seth Falcon <seth@opscode.com>
%% Copyright 2012 Opscode, Inc. All Rights Reserved.
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


-module(chef_wm_base).

%% Complete webmachine callbacks
-export([content_types_accepted/2,
         content_types_provided/2,
         finish_request/2,
         forbidden/2,
         is_authorized/2,
         malformed_request/2,
         ping/2,
         post_is_create/2,
         service_available/2]).

%% Default functions available to mixin
-export([auth_info/2,
         validate_request/3]).

%% Helpers for webmachine callbacks
-export([create_from_json/5,
         init/2,
         log_request/2,
         verify_request_signature/2,
         update_from_json/4]).

%% "Grab Bag" functions that will also need to be implemented by other base resources
-export([check_cookbook_authz/3,
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
-include_lib("eunit/include/eunit.hrl").

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
                batch_size = ?gv(batch_size, InitParams),
                auth_skew = ?gv(auth_skew, InitParams),
                resource_mod = ResourceMod}.

%% @doc Determines if service is available.
%%
%% Also initializes chef_db_context and reqid fields of base_state.
%% And handle other base_state init that depends on `Req'.
service_available(Req, State) ->
    %% TODO: query overload here and send 503 also can consult
    %% config/darklaunch to determine if we are in maint mode.
    State0 = set_req_contexts(Req, State),
    State1 = State0#base_state{organization_name = ?OSC_ORG_NAME,
                               organization_guid = ?OSC_ORG_ID},
    spawn_stats_hero_worker(Req, State1),
    {_GetHeader, State2} = chef_wm_util:get_header_fun(Req, State1),
    {true, Req, State2}.

validate_request(_Verb, Req, State) ->
    {Req, State}.

auth_info(Req, State) ->
    {not_found, Req, State}.

post_is_create(Req, State) ->
    {true, Req, State}.

malformed_request(Req, #base_state{resource_mod=Mod,
                                   auth_skew=AuthSkew}=State) ->
    {GetHeader, State1} = chef_wm_util:get_header_fun(Req, State),
    try
        chef_authn:validate_headers(GetHeader, AuthSkew),
        Req1 = body_not_too_big(Req),
        OrgId = fetch_org_guid(State1),
        {Req2, State2} = Mod:validate_request(wrq:method(Req1), Req1,
                                              State1#base_state{organization_guid = OrgId}),
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
                throw:bad_headers ->
            Msg1 =  chef_wm_malformed:malformed_request_message(bad_headers, Req, State),
            Req3 = wrq:set_resp_body(chef_json:encode(Msg1), Req),
            {{halt, 401}, Req3, State1#base_state{log_msg = bad_headers}};
        throw:bad_sign_desc ->
            Msg1 =  chef_wm_malformed:malformed_request_message(bad_sign_desc, Req, State),
            Req3 = wrq:set_resp_body(chef_json:encode(Msg1), Req),
            {{halt, 400}, Req3, State1#base_state{log_msg = bad_sign_desc}};
        throw:{too_big, Msg} ->
            error_logger:info_msg("json too large (~p)", [Msg]),
            Req3 = wrq:set_resp_body(chef_json:encode({[{<<"error">>, Msg}]}), Req),
            {{halt, 413}, Req3, State1#base_state{log_msg = too_big}};
        throw:Why ->
            Msg =  chef_wm_malformed:malformed_request_message(Why, Req, State),
            NewReq = wrq:set_resp_body(chef_json:encode(Msg), Req),
            {true, NewReq, State1#base_state{log_msg = Why}}
    end.

forbidden(Req, #base_state{resource_mod=Mod}=State) ->
    %% For now we call auth_info because currently need the side-effect of looking up the
    %% record and returning 404.
    case Mod:auth_info(Req, State) of
        {{halt, _}, _, _} = Halt ->
            Halt;
        {_, Req1, State1} ->
            case handle_auth_info(Mod, Req1, State1) of
                forbidden ->
                    {Req2, State2} = set_forbidden_msg(Req1, State1),
                    {true, Req2, State2};
                authorized ->
                    {false, Req1, State1}
            end
    end.

is_authorized(Req, State) ->
    case verify_request_signature(Req, State) of
        {true, Req1, State1} ->
            {true, Req1, State1};
        {false, ReqOther, StateOther} ->
            %% FIXME: the supported version is determined by the chef_authn application
            %% also, see: https://wiki.corp.opscode.com/display/CORP/RFC+Authentication+Version+Negotiation
            {"X-Ops-Sign version=\"1.0\" version=\"1.1\"", ReqOther, StateOther}
    end.

content_types_accepted(Req, State) ->
    {[{"application/json", from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

finish_request(Req, #base_state{reqid = ReqId}=State) ->
    try
        Code = wrq:response_code(Req),
        log_request(Req, State),
        stats_hero:report_metrics(ReqId, Code),
        stats_hero:stop_worker(ReqId),
        case Code of
            500 ->
                %% sanitize response body
                Msg = <<"internal service error">>,
                Json = chef_json:encode({[{<<"error">>, [Msg]}]}),
                Req1 = wrq:set_resp_header("Content-Type",
                                           "application/json", Req),
                {true, wrq:set_resp_body(Json, Req1), State};
            _ ->
                {true, Req, State}
        end
    catch
        X:Y ->
            error_logger:error_report({X, Y, erlang:get_stacktrace()})
    end.

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
                                     auth_skew = AuthSkew,
                                     chef_db_context = DbContext}=State) ->
    UserName = wrq:get_req_header("x-ops-userid", Req),
    case chef_db:fetch_requestor(DbContext, OrgName, UserName) of
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
create_from_json(#wm_reqdata{} = Req,
                 #base_state{chef_db_context = DbContext,
                             organization_guid = OrgId,
                             requestor_id = ActorId} = State,
                 RecType, {authz_id, AuthzId}, ObjectEjson) ->
    %% ObjectEjson should already be normalized. Record creation does minimal work and does
    %% not add or update any fields.
    ObjectRec = chef_object:new_record(RecType, OrgId, maybe_authz_id(AuthzId), ObjectEjson),
    Id = chef_object:id(ObjectRec),
    Name = chef_object:name(ObjectRec),
    TypeName = chef_object:type_name(ObjectRec),
    %% We send the object data to solr for indexing *first*. If it fails, we'll error out on
    %% a 500 and client can retry. If we succeed and the db call fails or conflicts, we can
    %% safely send a delete to solr since this is a new object with a unique ID unknown to
    %% the world.
    ok = chef_object_db:add_to_solr(TypeName, Id, OrgId,
                                 chef_object:ejson_for_indexing(ObjectRec, ObjectEjson)),
    case chef_db:create(ObjectRec, DbContext, ActorId) of
        {conflict, _} ->
            %% ignore return value of solr delete, this is best effort.
            chef_object_db:delete_from_solr(ObjectRec),
            %% FIXME: created authz_id is leaked for this case, cleanup?
            LogMsg = {RecType, name_conflict, Name},
            ConflictMsg = conflict_message(TypeName, Name),
            {{halt, 409}, chef_wm_util:set_json_body(Req, ConflictMsg),
             State#base_state{log_msg = LogMsg}};
        ok ->
            LogMsg = {created, Name},
            Uri = ?BASE_ROUTES:route(TypeName, Req, [{name, Name}]),
            {true,
             chef_wm_util:set_uri_of_created_resource(Uri, Req),
             State#base_state{log_msg = LogMsg}};
        What ->
            %% ignore return value of solr delete, this is best effort.
            %% FIXME: created authz_id is leaked for this case, cleanup?
            chef_object_db:delete_from_solr(ObjectRec),
            {{halt, 500}, Req, State#base_state{log_msg = What}}
    end.

-spec update_from_json(#wm_reqdata{},
                       #base_state{},
                       chef_object() | #chef_cookbook_version{} | #chef_user{},
                       ejson_term()) ->
                              {true, #wm_reqdata{}, #base_state{}} |
                              {{halt, 400 | 404 | 500}, #wm_reqdata{}, #base_state{}}.
%% @doc Implements the from_json callback for PUT requests to update Chef
%% objects. `OrigObjectRec' should be the existing and unmodified `chef_object()'
%% record. `ObjectEjson' is the parsed EJSON from the request body.
update_from_json(#wm_reqdata{} = Req, #base_state{chef_db_context = DbContext,
                                                  organization_guid = OrgId,
                                                  requestor_id = ActorId}=State,
                 OrigObjectRec, ObjectEjson) ->
    ObjectRec = chef_object:update_from_ejson(OrigObjectRec, ObjectEjson),
    %% Send object to solr for indexing *first*. If the update fails, we will have sent
    %% incorrect data, but that should get corrected when the client retries. This is a
    %% compromise.
    ok = chef_object_db:add_to_solr(chef_object:type_name(ObjectRec),
                                    chef_object:id(ObjectRec),
                                    OrgId,
                                    chef_object:ejson_for_indexing(ObjectRec, ObjectEjson)),

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
            UpdateFun = chef_db:update_fun(ObjectRec),
            case chef_db:UpdateFun(DbContext, ObjectRec, ActorId) of
                ok ->
                    Req1 = handle_rename(ObjectRec, Req),
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
                    TypeName = chef_object:type_name(ObjectRec),
                    RecType = erlang:element(1,ObjectRec),
                    LogMsg = {RecType, name_conflict, Name},
                    ConflictMsg = conflict_message(TypeName, Name),
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

-spec delete_object(chef_db:db_context(),
                    chef_object() | #chef_cookbook_version{},
                    object_id()) -> ok.
delete_object(DbContext, Object, RequestId) ->
    chef_object_db:delete(DbContext, Object, RequestId).

-spec check_cookbook_authz(Cookbooks :: [#chef_cookbook_version{}],
                           Req :: wm_req(),
                           State :: #base_state{}) ->
                                  ok | {error, Msg :: binary()}.
check_cookbook_authz(_, _, #base_state{}) ->
    case random:uniform(1) of
        1 ->
            ok;
        _ ->
            {error, <<"makes dialyzer happy">>}
    end.

conflict_message(cookbook_version, _Name) ->
    {[{<<"error">>, [<<"Cookbook already exists">>]}]};
conflict_message(role, _Name) ->
    {[{<<"error">>, [<<"Role already exists">>]}]};
conflict_message(node, _Name) ->
    %% Msg = iolist_to_binary([<<"A node named '">>, Name, <<"' already exists.">>]),
    Msg = <<"Node already exists">>,
    {[{<<"error">>, [Msg]}]};
conflict_message(data_bag_item, {BagName, ItemName}) ->
    Msg = <<"Data Bag Item '", ItemName/binary, "' already exists in Data Bag '",
            BagName/binary, "'.">>,
    {[{<<"error">>, [Msg]}]};
conflict_message(data_bag, _Name) ->
    %% {[{<<"error">>, [<<"Data Bag '", Name/binary, "' already exists">>]}]}.
    {[{<<"error">>, [<<"Data bag already exists">>]}]};
conflict_message(environment, _Name) ->
    {[{<<"error">>, [<<"Environment already exists">>]}]};
conflict_message(client, _Name) ->
    {[{<<"error">>, [<<"Client already exists">>]}]}.

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

set_req_contexts(Req, #base_state{reqid_header_name = HeaderName} = State) ->
    ReqId = read_req_id(HeaderName, Req),
    DbContext = chef_db:make_context(ReqId),
    State#base_state{chef_db_context = DbContext, reqid = ReqId}.

read_req_id(ReqHeaderName, Req) ->
    case wrq:get_req_header(ReqHeaderName, Req) of
        undefined ->
            base64:encode(crypto:md5(term_to_binary(make_ref())));
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

log_request(Req, #base_state{reqid = ReqId, log_msg = Msg, organization_name = Org}) ->
    Status = wrq:response_code(Req),
    Tuples = [{req_id, ReqId},
              {status, Status},
              {method, wrq:method(Req)},
              {path, wrq:raw_path(Req)},
              {user, wrq:get_req_header("x-ops-userid", Req)},
              {msg, {raw, Msg}}],
    PerfTuples = stats_hero:snapshot(ReqId, agg),
    Level = log_level(Status),
    fast_log:Level(erchef, maybe_add_org_name(Org, Tuples) ++ PerfTuples).

log_level(Code) when Code >= 500 ->
    err;
log_level(_) ->
    info.

fetch_org_guid(#base_state{organization_guid = Id}) when is_binary(Id) ->
    Id;
fetch_org_guid(#base_state{organization_guid = undefined,
                           organization_name = OrgName,
                           chef_db_context = DbContext}) ->
    case chef_db:fetch_org_id(DbContext, OrgName) of
        not_found -> throw({org_not_found, OrgName});
        Guid -> Guid
    end.

maybe_add_org_name(?OSC_ORG_NAME, Items) ->
    Items;
maybe_add_org_name(OrgName, Items) ->
    [{org_name, OrgName} | Items].

%% If request results in a rename, then set Location header and wm will return with a 201.
%% Currently, only the clients endpoint supports rename
handle_rename(#chef_client{name = ObjectName}, Req) ->
    ReqName = chef_wm_util:object_name(client, Req),
    case ObjectName of
        ReqName ->
            Req;
        _ ->
            Uri = ?BASE_ROUTES:route(client, Req, [{name, ObjectName}]),
            wrq:set_resp_header("Location", binary_to_list(Uri), Req)
    end;
handle_rename(_, Req) ->
    Req.

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
                            throw:badarg ->
                                error_logger:error_report({"unknown webkey tag", Tag,
                                                           erlang:get_stacktrace()}),
                                %% alternately, we could just use the default key instead of failing;
                                %% but I prefer noisy errors
                                throw({badarg, "unknown webkey tag", Tag})
                        end
                end,
            case chef_keyring:get_key(WebKeyTag) of
                {ok, Key} ->
                    Key;
                {error, unknown_key} ->
                    Msg = io_lib:format("Failed finding key ~w", [WebKeyTag]),
                    error_logger:error_report({no_such_key, Msg, erlang:get_stacktrace()}),
                    throw({no_such_key, WebKeyTag})
            end;
        _Else ->
            public_key(Requestor)
    end.

-spec body_not_too_big(#wm_reqdata{}) -> #wm_reqdata{}.
%% Verify that the request body is not larger than ?MAX_SIZE bytes. Throws `{too_big, Msg}`
%% if the request body is too large.
body_not_too_big(Req) ->
    body_not_too_big(wrq:method(Req), wrq:set_max_recv_body(?MAX_SIZE, Req)).

body_not_too_big(Method, Req) when Method =:= 'POST';
                                   Method =:= 'PUT' ->
    try
        %% Force a read of request body. Webmachine memoizes this in the process
        %% dictionary. Webmachine will read in chunks and call exit/1 if the body exceeds
        %% the max set above. It would be nice if there was something other than a string to
        %% match against. TODO: patch webmachine.
        wrq:req_body(Req),
        Req
    catch
        exit:"request body too large" ->
            Msg = iolist_to_binary([<<"JSON must be no more than ">>,
                                    integer_to_list(?MAX_SIZE),
                                    <<" bytes.">>]),
            throw({too_big, Msg})
    end;
body_not_too_big(_Method, Req) ->
    Req.

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

%%
%% forbidden helpers
%%
-spec handle_auth_info(atom(), wm_req(), #base_state{}) -> authorized | forbidden.
handle_auth_info(chef_wm_clients, Req,
                 #base_state{requestor = Requestor,
                             resource_state = #client_state{client_data = Client}}) ->
    case wrq:method(Req) of
        'POST' -> %% create
            NotCreatingAdmin = ej:get({<<"admin">>}, Client) =/= true,
            IsAdmin = chef_wm_authz:is_admin(Requestor),
            IsValidator = chef_wm_authz:is_validator(Requestor),
            case IsAdmin orelse (IsValidator andalso NotCreatingAdmin) of
                true -> authorized;
                false -> forbidden
            end;
        'GET' -> %% index
            chef_wm_authz:allow_admin(Requestor);
        _Else ->
            forbidden
    end;
handle_auth_info(chef_wm_named_client, Req, #base_state{requestor = Requestor,
                                                        resource_state =
                                                            #client_state{chef_client = Client}}) ->
    ClientName = chef_wm_util:object_name(client, Req),
    case wrq:method(Req) of
        'PUT' -> %% update
            chef_wm_authz:allow_admin(Requestor);
        'GET' -> %% show
            chef_wm_authz:allow_admin_or_requesting_node(Requestor, ClientName);
        'DELETE' -> %% delete
            #chef_client{validator = IsValidator} = Client,
            case IsValidator of
                true -> %% We can't delete the validator
                    forbidden;
                _Else ->
                    chef_wm_authz:allow_admin_or_requesting_node(Requestor, ClientName)
            end;
        _Else ->
            forbidden
    end;
handle_auth_info(chef_wm_users, _, _) ->
    %% temp pass
    authorized;
handle_auth_info(chef_wm_named_user, _, _) ->
    %% temp pass
    authorized;
handle_auth_info(Module, Req, #base_state{requestor = Requestor})
        when Module =:= chef_wm_cookbook_version;
             Module =:= chef_wm_named_environment;
             Module =:= chef_wm_named_role;
             Module =:= chef_wm_named_data_item ->
    case wrq:method(Req) of
        'GET' ->
            authorized;
        'PUT' -> %% update
            chef_wm_authz:allow_admin(Requestor);
        'DELETE' ->
            chef_wm_authz:allow_admin(Requestor);
        _Else ->
            forbidden
    end;
handle_auth_info(Module, Req, #base_state{requestor = Requestor})
        when Module =:= chef_wm_data;
             Module =:= chef_wm_environments;
             Module =:= chef_wm_roles;
             Module =:= chef_wm_sandboxes ->
    case wrq:method(Req) of
        'GET' ->
            authorized;
        'POST' -> %% create
            chef_wm_authz:allow_admin(Requestor);
        _Else ->
            forbidden
    end;
handle_auth_info(chef_wm_named_data, Req, #base_state{requestor = Requestor}) ->
    case wrq:method(Req) of
        'GET' ->
            authorized;
        'POST' -> %% create data_item
            chef_wm_authz:allow_admin(Requestor);
        'DELETE' -> %% delete data
            chef_wm_authz:allow_admin(Requestor);
        _Else ->
            forbidden
    end;
handle_auth_info(chef_wm_named_node, Req, #base_state{requestor = Requestor}) ->
    NodeName = chef_wm_util:object_name(node, Req),
    case wrq:method(Req) of
        'GET' ->
            authorized;
        'PUT' -> %% update
            chef_wm_authz:allow_admin_or_requesting_node(Requestor, NodeName);
        'DELETE' -> %% delete
            chef_wm_authz:allow_admin_or_requesting_node(Requestor, NodeName);
        _Else ->
            forbidden
    end;
handle_auth_info(Module, Req, _State)
        when Module =:= chef_wm_nodes;
             Module =:= chef_wm_search ->
    case wrq:method(Req) of
        'GET' ->
            authorized;
        'POST' ->
            authorized;
        _Else ->
            forbidden
    end;
handle_auth_info(Module, Req, _State)
        when Module =:= chef_wm_cookbooks;
             Module =:= chef_wm_environment_cookbooks;
             Module =:= chef_wm_environment_recipes;
             Module =:= chef_wm_environment_roles;
             Module =:= chef_wm_search_index;
             Module =:= chef_wm_status ->
    case wrq:method(Req) of
        'GET' ->
            authorized;
        _Else ->
            forbidden
    end;
handle_auth_info(chef_wm_depsolver, Req, _State) ->
    case wrq:method(Req) of
        'POST' ->
            authorized;
        _Else ->
            forbidden
    end;
handle_auth_info(chef_wm_named_sandbox, Req, #base_state{requestor = Requestor}) ->
    case wrq:method(Req) of
        'PUT' ->
            chef_wm_authz:allow_admin(Requestor);
        _Else ->
            forbidden
    end;
%% Default case is to allow disallow all requests
handle_auth_info(_Mod, _Req, _State) ->
    forbidden.

set_forbidden_msg(Req, State) ->
    Msg = <<"You are not allowed to take this action.">>,
    JsonMsg = chef_json:encode({[{<<"error">>, [Msg]}]}),
    Req1 = wrq:set_resp_body(JsonMsg, Req),
    {Req1, State#base_state{log_msg = {forbidden}}}.
