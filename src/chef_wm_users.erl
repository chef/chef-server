%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Mark Mzyk <mmzyk@opscode.com>
%% @copyright 2012 Opscode, Inc.

%% @doc Resource module for Chef users endpoint

-module(chef_wm_users).

-include("chef_wm.hrl").

-mixin([{chef_wm_base, [content_types_accepted/2,
                        content_types_provided/2,
                        finish_request/2,
                        malformed_request/2,
                        ping/2,
                        post_is_create/2]}]).

-mixin([{?BASE_RESOURCE, [forbidden/2,
                          is_authorized/2,
                          service_available/2]}]).

-behavior(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3]).

-export([allowed_methods/2,
         create_path/2,
         from_json/2,
         resource_exists/2,
         to_json/2]).

init(Config) ->
  chef_wm_base:init(?MODULE, Config).

%% Need to add the user_state
init_resource_state(_Config) ->
  {ok, #user_state{}}.

request_type() ->
  "users".

allowed_methods(Req, State) ->
  {['GET', 'POST'], Req, State}.

validate_request('POST', Req, State) ->
  case wrq:req_body(Req) of
    undefined ->
      throw({error, missing_body});
   Body ->
      {ok, UserData} = chef_user:parse_binary_json(Body),
      {Req, State#base_state{resource_state =
          #user_state{user_data = UserData}}}
  end;
validate_request('GET', Req, State) ->
    {Req, State}.

%% Create, destroy, and update are admin only actions
%% Need to update this to reflect that, as right now it
%% lets anyone through.
auth_info(Req, State) ->
  {authorized, Req, State}.

%% If we get here, are we guarenteed the user exists?
resource_exists(Req, State) ->
  {true, Req, State}.

%% What is the purpose of this method?
create_path(Req, #base_state{resource_state = #user_state{user_data = UserData}} = State) ->
  Name = ej:get({<<"name">>}, UserData),
  {binary_to_list(Name), Req, State}.

from_json(Req, #base_state{reqid = RequestId,
                           resource_state = #user_state{user_data = UserData,
                           user_authz_id = AuthzId}} = State) ->
    Name = ej:get({<<"name">>}, UserData),
    {PublicKey, PrivateKey} = case chef_object_base:cert_or_key(UserData) of
        {undefined, _} ->
            chef_wm_util:generate_keypair(Name, RequestId);
        {PubKey, _PubKeyVersion} ->
            {PubKey, undefined}
    end,
    UserWithKey = chef_object_base:set_public_key(UserData, PublicKey),
    PasswordData = chef_wm_password:encrypt(ej:get({<<"password">>}, UserWithKey)),
    case create_from_json(Req, State, chef_user, {authz_id, AuthzId},
                          {UserWithKey, PasswordData}) of
        {true, Req1, State1} ->
            Uri = ?BASE_ROUTES:route(user, Req1, [{name, Name}]),
            Ejson = chef_object_base:set_key_pair({[{<<"uri">>, Uri}]},
                        {public_key, PublicKey}, {private_key, PrivateKey}),
            {true, chef_wm_util:set_json_body(Req1, Ejson), State1};
        Else ->
            Else
    end.

to_json(Req, State) ->
    {all_users_json(Req, State), Req, State}.

%% Internal Functions
all_users_json(Req, #base_state{chef_db_context = DbContext}) ->
    UserNames = chef_db:fetch_users(DbContext),
    RouteFun = ?BASE_ROUTES:bulk_route_fun(user, Req),
    UriMap = [ {Name, RouteFun(Name)} || Name <- UserNames ],
    ejson:encode({UriMap}).

%% FIXME: we will likely be able to re-use something from chef_wm_base once a bit of
%% refaactoring happens. This is largely copy pasta from chef_wm_base, but with solr bits
%% removed.
create_from_json(#wm_reqdata{} = Req,
                 #base_state{chef_db_context = DbContext,
                             organization_guid = OrgId,
                             requestor_id = ActorId} = State,
                 RecType, {authz_id, AuthzId}, ObjectEjson) ->
    %% ObjectEjson should already be normalized. Record creation does minimal work and does
    %% not add or update any fields.
    ObjectRec = chef_object:new_record(RecType, OrgId, maybe_authz_id(AuthzId), ObjectEjson),
    Name = chef_object:name(ObjectRec),
    case chef_db:create(ObjectRec, DbContext, ActorId) of
        {conflict, _} ->
            %% FIXME: created authz_id is leaked for this case, cleanup?
            LogMsg = {RecType, name_conflict, Name},
            ConflictMsg = conflict_message(Name),
            {{halt, 409}, chef_wm_util:set_json_body(Req, ConflictMsg),
             State#base_state{log_msg = LogMsg}};
        ok ->
            LogMsg = {created, Name},
            Uri = ?BASE_ROUTES:route(user, Req, [{name, Name}]),
            {true,
             chef_wm_util:set_uri_of_created_resource(Uri, Req),
             State#base_state{log_msg = LogMsg}};
        What ->
            {{halt, 500}, Req, State#base_state{log_msg = What}}
    end.

maybe_authz_id(undefined) ->
    unset;
maybe_authz_id(B) ->
    B.

conflict_message(Name) ->
    Msg = iolist_to_binary([<<"User '">>, Name, <<"' already exists">>]),
    {[{<<"error">>, [Msg]}]}.

error_message(Msg) when is_list(Msg) ->
    error_message(iolist_to_binary(Msg));
error_message(Msg) when is_binary(Msg) ->
    {[{<<"error">>, [Msg]}]}.

malformed_request_message(#ej_invalid{type = json_type, key = Key}, _Req, _State) ->
    case Key of
        undefined -> error_message([<<"Incorrect JSON type for request body">>]);
        _ ->error_message([<<"Incorrect JSON type for ">>, Key])
    end;
malformed_request_message(#ej_invalid{type = missing, key = Key}, _Req, _State) ->
    error_message([<<"Required value for ">>, Key, <<" is missing">>]);
malformed_request_message({invalid_key, Key}, _Req, _State) ->
    error_message([<<"Invalid key ">>, Key, <<" in request body">>]);
malformed_request_message(invalid_json_body, _Req, _State) ->
    error_message([<<"Incorrect JSON type for request body">>]);
malformed_request_message(#ej_invalid{type = exact, key = Key, msg = Expected},
                          _Req, _State) ->
    error_message([Key, <<" must equal ">>, Expected]);
malformed_request_message(#ej_invalid{type = fun_match, key = Key, msg = Error},
                          _Req, _State) when Key =:= <<"password">> ->
    error_message([Error]);
malformed_request_message(#ej_invalid{type = string_match, msg = Error}, _Req, _State) ->
    error_message([Error]);
malformed_request_message(#ej_invalid{type = object_key, key = Object, found = Key},
                          _Req, _State) ->
    error_message([<<"Invalid key '">>, Key, <<"' for ">>, Object]);
% TODO: next two tests can get merged (hopefully) when object_map is extended not
% to swallow keys
malformed_request_message(#ej_invalid{type = object_value, key = Object, found = Val},
                          _Req, _State) when is_binary(Val) ->
    error_message([<<"Invalid value '">>, Val, <<"' for ">>, Object]);
malformed_request_message(#ej_invalid{type = object_value, key = Object, found = Val},
                          _Req, _State) ->
    error_message([<<"Invalid value '">>, io_lib:format("~p", [Val]),
                   <<"' for ">>, Object]);
malformed_request_message(Any, Req, State) ->
    chef_wm_util:malformed_request_message(Any, Req, State).
