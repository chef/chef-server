%% NOTE: this is the stub for the OSC wm base module
-module(chef_wm_base).

%% Complete webmachine callbacks
-export([ping/2,
         is_authorized/2,
         content_types_provided/2,
         service_available/2,
         finish_request/2,
         malformed_request/2]).

%% Complete webmachine callbacks which must be
%% mixed in and aliased to the proper name.
-export([forbidden_on_object_type/2,
         forbidden_on_object/2,
         selectable_forbidden/2]).

%% Default functions available to mixin
-export([validate_request/3,
         auth_info/2]).

%% Helpers for webmachine callbacks
-export([init/2,
         create_from_json/5,
         log_request/2,
         base_uri/1,
         update_from_json/4]).

%% Can't use callback specs to generate behaviour_info because webmachine.hrl
%% contains a function definition.

%% -callback validate_request(atom(), #wm_reqdata{}, any()) -> {#wm_reqdata{}, any()}.
%% -callback malformed_request_message(any(), #wm_reqdata{}, any()) -> {[{binary(), [binary()]}]}.
%% -callback request_type() -> string().
%% -callback auth_info(#wm_reqdata{}, any()) -> {not_found | binary(), #wm_reqdata{}, any()}.

-define(NO_PING, no_ping).
-include("chef_wm.hrl").

init(ResourceMod, Config) ->
    {ok, init_base_state(ResourceMod, Config)}.

ping(Req, State) ->
    {pong, Req, State}.

init_base_state(ResourceMod, InitParams) ->
    #base_state{reqid_header_name = ?gv(reqid_header_name, InitParams),
                batch_size = ?gv(batch_size, InitParams),
                auth_skew = ?gv(auth_skew, InitParams),
                db_type = ?gv(db_type, InitParams),
                organization_guid = <<"">>,
                organization_name = <<"">>,
                resource_mod = ResourceMod}.

validate_request(_Verb, Req, State) ->
    {Req, State}.

auth_info(Req, State) ->
    {not_found, Req, State}.

%% @doc Determines if service is available.
%%
%% Also initializes chef_db_context and reqid fields of base_state.
%% And handle other base_state init that depends on `Req'.
service_available(Req, State0) ->
    %% TODO: query overload here and send 503 also can consult
    %% config/darklaunch to determine if we are in maint mode.
    State1 = set_chef_db_context(Req, State0),
    {_GetHeader, State2} = get_header_fun(Req, State1),
    {true, Req, State2}.

malformed_request(Req, #base_state{resource_mod=Mod,
                                   auth_skew=AuthSkew}=State) ->
    {GetHeader, State1} = get_header_fun(Req, State),
    try
        chef_authn:validate_headers(GetHeader, AuthSkew),
        {Req1, State1} = Mod:validate_request(wrq:method(Req), Req, State),
        {false, Req1, State1}
    catch
        throw:{json_too_large, Msg} ->
            Req2 = wrq:set_resp_body(ejson:encode({[{<<"error">>, Msg}]}), Req),
            {{halt, 413}, Req2, State1#base_state{log_msg = json_too_large}};
        throw:bad_clock ->
            Msg1 = malformed_request_message(bad_clock, Req, State),
            Req2 = wrq:set_resp_body(ejson:encode(Msg1), Req),
            {{halt, 401}, Req2, State1#base_state{log_msg = bad_clock}};
        throw:Why ->
            Msg = malformed_request_message(Why, Req, State),
            NewReq = wrq:set_resp_body(ejson:encode(Msg), Req),
            {true, NewReq, State1#base_state{log_msg = Why}}
    end.


%% @doc Handle common malformed request tasks with resource-specific callbacks
%%
%% This function does a sanity check on the authn headers (not verifying signing, but does
%% all checks it can without talking to a db).  It also checks for org existence and halts
%% with 404 if the org is not found.  This doesn't strictly belong in malformed_request, but
%% right now all of our resources need this check and end up needing it before
%% resource_exists will get called so we do it here.
%%
%% The caller provides `ValidateFun' which will be given `Req' and `State' as args and
%% should return a `{Req, State}' tuple or throw.  The `ErrorMsgFun' will be called as
%% `ErrorMsgFun(Reason, Req, State)' where `Reason' is the term thrown by `ValidateFun'.
%%

malformed_request_message(bad_clock, Req, State) ->
    {GetHeader, _State1} = get_header_fun(Req, State),
    User = case GetHeader(<<"X-Ops-UserId">>) of
               undefined -> <<"">>;
               UID -> UID
           end,
    Msg = iolist_to_binary([<<"Failed to authenticate as ">>, User,
                            <<". Synchronize the clock on your host.">>]),
    {[{<<"error">>, [Msg]}]};
malformed_request_message({missing_headers, Missing}, _Req, _State) ->
    Msg = iolist_to_binary([
                            <<"missing required authentication header(s) ">>,
                            bin_str_join(Missing, <<", ">>)]),
    {[{<<"error">>, [Msg]}]};
malformed_request_message({invalid_json, _}, _Req, _State) ->
    %% in theory, there might be some sort of slightly useful error detail from ejson, but
    %% thus far nothing specific enough to beat out this. Also, would not passing internal
    %% library error messages out to the user when possible.
    {[{<<"error">>, [<<"invalid JSON">>]}]};
malformed_request_message({mismatch, {FieldName, _Pat, _Val}}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary(["Field '", FieldName, "' invalid"])]}]};
malformed_request_message({missing, FieldName}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary(["Field '", FieldName, "' missing"])]}]};

%% Not sure if we want to be this specific, or just want to fold this into an 'invalid JSON'
%% case.  At any rate, here it is.
malformed_request_message({bad_string_list, {Field, _Value}}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary(["Field '", Field, "' is not a list of strings"])]}]};
malformed_request_message({bad_ejson_proplist, {Field, _Value}}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary(["Field '", Field, "' is not a hash"])]}]};
malformed_request_message({url_json_name_mismatch, {_UrlName, _Mismatch, Type}}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary([Type, <<" name mismatch.">>])]}]};
malformed_request_message({bad_run_list, {Field, _Value}}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary(["Field '", Field, "' is not a valid run list"])]}]};
malformed_request_message({bad_run_lists, {Field, _Value}}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary(["Field '", Field, "' contains invalid run lists"])]}]};
malformed_request_message(Reason, Req, #base_state{resource_mod=Mod}=State) ->
    Mod:malformed_request_message(Reason, Req, State).

forbidden_on_object_type(Req, State) ->
    {false, Req, State}.

forbidden_on_object(Req, State) ->
    {false, Req, State}.

selectable_forbidden(Req, State) ->
    {false, Req, State}.

is_authorized(Req, State) ->
    {true, Req, State}.


content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

finish_request(Req, State) ->
    {true, Req, State}.

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
            {false, wrq:set_resp_body(ejson:encode(NotFoundMsg), Req),
             State#base_state{log_msg = {not_found, What}}};
        #chef_requestor{type = RequestorType0, key_data = KeyData0}=Requestor ->
            %% If the request originated from the webui, we do authn using the webui public
            %% key, not the user's key.
            {RequestorType, KeyData} = select_user_or_webui_key(Req, KeyData0, RequestorType0),
            Body = body_or_default(Req, <<>>),
            HTTPMethod = iolist_to_binary(atom_to_list(wrq:method(Req))),
            Path = iolist_to_binary(wrq:path(Req)),
            {GetHeader, State1} = get_header_fun(Req, State),
            case chef_authn:authenticate_user_request(GetHeader, HTTPMethod,
                                                      Path, Body, KeyData,
                                                      AuthSkew) of
                {name, _} ->
                    {true, Req,
                     %% FIXME: teach users of this code to get requestor type from requestor
                     %% record.
                     State1#base_state{requester_type = RequestorType,
                                       requestor = Requestor}};
                {no_authn, Reason} ->
                    Msg = verify_request_message(Reason, UserName, OrgName),
                    Json = ejson:encode(Msg),
                    Req1 = wrq:set_resp_body(Json, Req),
                    {false, Req1, State1#base_state{log_msg = Reason}}
            end
    end.

-spec create_from_json(Req :: #wm_reqdata{}, State :: #base_state{},
                       RecType :: chef_object_name(),
                       ContainerId ::object_id(), ObjectEjson :: ejson_term()) ->
                              {true | {halt, 409 | 500}, #wm_reqdata{}, #base_state{}}.
%% @doc Implements the from_json callback for POST requests to create Chef
%% objects. `RecType' is the name of the object record being created
%% (e.g. `chef_node'). `ContainerId' is the AuthzID of the container for the object being
%% created (e.g. node container authz ID for creating a node). The `ObjectEjson' is the
%% validated and normalized EJSON that was parsed from the request body.
create_from_json(#wm_reqdata{} = Req,
                 #base_state{chef_db_context = DbContext,
                             organization_guid = OrgId,
                             requestor = #chef_requestor{authz_id = ActorId},
                             db_type = DbType} = State,
                 RecType, _ContainerId, ObjectEjson) ->
    %% ObjectEjson should already be normalized. Record creation does minimal work and does
    %% not add or update any fields.
    ObjectRec = chef_object:new_record(RecType, OrgId, <<"">>, ObjectEjson,
                                       DbType),
    Id = chef_object:id(ObjectRec),
    Name = chef_object:name(ObjectRec),
    TypeName = chef_object:type_name(ObjectRec),
    %% We send the object data to solr for indexing *first*. If it fails, we'll error out on
    %% a 500 and client can retry. If we succeed and the db call fails or conflicts, we can
    %% safely send a delete to solr since this is a new object with a unique ID unknown to
    %% the world.
    ok = chef_object:add_to_solr(TypeName, Id, OrgId,
                                 chef_object:ejson_for_indexing(ObjectRec, ObjectEjson)),
    CreateFun = chef_object:create_fun(ObjectRec),
    case chef_db:CreateFun(DbContext, ObjectRec, ActorId) of
        {conflict, _} ->
            %% ignore return value of solr delete, this is best effort.
            chef_object:delete_from_solr(ObjectRec),

            LogMsg = {RecType, name_conflict, Name},
            ConflictMsg = conflict_message(TypeName, Name),
            {{halt, 409}, chef_rest_util:set_json_body(Req, ConflictMsg),
             State#base_state{log_msg = LogMsg}};
        ok ->
            LogMsg = {created, Name},
            Uri = chef_rest_routes:route(TypeName, Req, [{name, Name}]),
            {true,
             chef_rest_util:set_uri_of_created_resource(Uri, Req),
             State#base_state{log_msg = LogMsg}};
        What ->
            %% ignore return value of solr delete, this is best effort.
            chef_object:delete_from_solr(ObjectRec),
            {{halt, 500}, Req, State#base_state{log_msg = What}}
    end.

-spec update_from_json(#wm_reqdata{}, #base_state{}, chef_object(), ejson_term()) ->
                              {true, #wm_reqdata{}, #base_state{}} |
                              {{halt, 404 | 500}, #wm_reqdata{}, #base_state{}}.
%% @doc Implements the from_json callback for PUT requests to update Chef
%% objects. `OrigObjectRec' should be the existing and unmodified `chef_object()'
%% record. `ObjectEjson' is the parsed EJSON from the request body.
update_from_json(#wm_reqdata{} = Req, #base_state{chef_db_context = DbContext,
                                  organization_guid = OrgId,
                                  requestor = #chef_requestor{authz_id = ActorId},
                             db_type = DbType}=State, OrigObjectRec, ObjectEjson) ->
    ObjectRec = chef_object:update_from_ejson(OrigObjectRec, ObjectEjson, DbType),
    %% Send object to solr for indexing *first*. If the update fails, we will have sent
    %% incorrect data, but that should get corrected when the client retries. This is a
    %% compromise.
    ok = chef_object:add_to_solr(chef_object:type_name(ObjectRec),
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
            {true, chef_rest_util:set_json_body(Req, ObjectEjson), State1};
        false ->
            UpdateFun = chef_object:update_fun(ObjectRec),
            case chef_db:UpdateFun(DbContext, ObjectRec, ActorId) of
                ok ->
                    {true, chef_rest_util:set_json_body(Req, ObjectEjson), State};
                not_found ->
                    %% We will get this if no rows were affected by the query. This could
                    %% happen if the object is deleted in the middle of handling this
                    %% request. In this case, we return 404 just as we would if the client
                    %% retried.
                    State1 = State#base_state{log_msg = not_found},
                    Msg = chef_rest_util:not_found_message(chef_object:type_name(ObjectRec),
                                                           chef_object:name(ObjectRec)),
                    Req1 = chef_rest_util:set_json_body(Req, Msg),
                    {{halt, 404}, Req1, State1};
                Why ->
                    State1 = State#base_state{log_msg = Why},
                    {{halt, 500}, Req, State1}
            end
    end.

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
    {[{<<"error">>, [<<"Data bag already exists">>]}]}.

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

set_chef_db_context(Req, #base_state{reqid_header_name = HeaderName} = State) ->
    ReqId = read_req_id(HeaderName, Req),
    Context = chef_db:make_context(ReqId),
    State#base_state{chef_db_context = Context, reqid = ReqId}.

read_req_id(ReqHeaderName, Req) ->
    case wrq:get_req_header(ReqHeaderName, Req) of
        undefined ->
            base64:encode(term_to_binary(make_ref()));
        HV ->
            iolist_to_binary(HV)
    end.

get_header_fun(Req, State = #base_state{header_fun = HFun})
  when HFun =:= undefined ->
    GetHeader = fun(H) ->
                        Name = case is_binary(H) of
                                   true -> binary_to_list(H);
                                   false -> H
                               end,
                        case wrq:get_req_header(string:to_lower(Name), Req) of
                            B when is_binary(B) -> B;
                            S when is_list(S) -> iolist_to_binary(S);
                            undefined -> undefined
                        end
                end,
    {GetHeader, State#base_state{header_fun = GetHeader}};
get_header_fun(_Req, State) ->
    {State#base_state.header_fun, State}.


log_request(Req, #base_state{reqid = ReqId, log_msg = Msg, organization_name = Org}) ->
    Status = wrq:response_code(Req),
    Tuples = [{req_id, ReqId},
              {status, Status},
              {org_name, Org},
              {method, wrq:method(Req)},
              {path, wrq:raw_path(Req)},
              {user, wrq:get_req_header("x-ops-userid", Req)},
              {msg, {raw, Msg}}],
    PerfTuples = stats_hero:snapshot(ReqId, agg),
    Level = log_level(Status),
    fast_log:Level(erchef, Tuples ++ PerfTuples).

log_level(Code) when Code >= 500 ->
    err;
log_level(_) ->
    info.

bin_str_join(L, Sep) ->
    bin_str_join(L, Sep, []).

bin_str_join([H], _Sep, Acc) ->
    lists:reverse([<<"'">>, H, <<"'">>|Acc]);
bin_str_join([H | T], Sep, Acc) ->
    bin_str_join(T, Sep, [Sep, <<"'">>, H, <<"'">> | Acc]).

%%% @doc Return appropriate public key based on request source
%%%
%%% Requests coming from the webui, marked by the 'X-Ops-Request-Source' header read the
%%% webui public key and use that for authn. Otherwise this function just passes through the
%%% "KeyData" arg which is the user or client public key.
%%%
%%% The webui public key is fetched from the chef_keyring service. The 'X-Ops-WebKey-Tag'
%%% header specifies which key id to use, or we use the 'default' key if it is missing.
%%%
select_user_or_webui_key(Req, KeyData, RequestorType) ->
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
                    {webui, Key};
                {error, unknown_key} ->
                    Msg = io_lib:format("Failed finding key ~w", [WebKeyTag]),
                    error_logger:error_report({no_such_key, Msg, erlang:get_stacktrace()}),
                    throw({no_such_key, WebKeyTag})
            end;
        _Else ->
            {RequestorType, KeyData}
    end.

%% @doc Returns the base URI for the server as called by the client as a string.
base_uri(Req) ->
    Scheme = scheme(Req),
    Host = string:join(lists:reverse(wrq:host_tokens(Req)), "."),
    PortString = port_string(wrq:port(Req)),
    Scheme ++ "://" ++ Host ++ PortString.

scheme(Req) ->
    case wrq:get_req_header("x-forwarded-proto", Req) of
        undefined ->
            case wrq:scheme(Req) of
                https -> "https";
                http -> "http";
                P -> erlang:atom_to_list(P)
            end;
        Proto -> Proto
    end.

%% So this is kind of gross and will prevent correct port info if you run https on port 80
%% or http on port 443; otherwise it should work. The problem is two-fold, first webmachine
%% ignores scheme information when parsing the host header and so always sets the port to 80
%% if no port is present in the host header. But in a load-balanced situation, the scheme
%% from webmachine may not reflect what is in use at the load balancer. A simple compromise
%% is to treat both 80 and 443 as default and only include a port string if the port differs
%% from those.
port_string(Default) when Default =:= 80; Default =:= 443 ->
    "";
port_string(Port) ->
    [$:|erlang:integer_to_list(Port)].

