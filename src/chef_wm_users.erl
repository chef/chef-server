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
      {ok, UserData} = chef_user:parse_binary_json(Body, create),
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
  %% FIXME: This code closely mirrors that of client and can likely be extracted out
  Name = ej:get({<<"name">>}, UserData),
  {PublicKey, PrivateKey} = chef_wm_util:generate_keypair(Name, RequestId),
  UserData1 = chef_user:set_public_key(UserData, PublicKey),
  UserData2 = secure_password(UserData1),
  case chef_wm_base:create_from_json(Req, State, chef_user, {authz_id, AuthzId}, UserData2) of
    {true, Req1, State1} ->
      Uri = list_to_binary(chef_wm_util:full_uri(Req1)),
      Ejson = {[{<<"uri">>, Uri},
                {<<"private_key">>, PrivateKey},
                {<<"public_key">>, PublicKey}
               ]},
      {true, chef_wm_util:set_json_body(Req1, Ejson), State1};
    Else ->
      Else
  end.

secure_password(User) ->
  _Password = ej:get({<<"password">>}, User),
 %% Password gets passed to the password module here
 %% Assuming return type from password module, will need to adjust
 %% Using hard coded values for now
 {HashedPassword, Salt, HashType} = {<<"hidden">>, <<"kosher">>, <<"thegoodkind">>},
 User1 = ej:set({<<"password">>}, User, HashedPassword),
 User2 = ej:set({<<"salt">>}, User1, Salt),
 User3 = ej:set({<<"hash_type">>}, User2, HashType),
 User3.

%% Need to write function to be called here
to_json(Req, State) ->
    {all_users_json(Req, State), Req, State}.

%% Internal Functions
all_users_json(Req, #base_state{chef_db_context = DbContext}) ->
    UserNames = chef_db:fetch_users(DbContext),
    RouteFun = ?BASE_ROUTES:bulk_route_fun(user, Req),
    UriMap = [ {Name, RouteFun(Name)} || Name <- UserNames ],
    ejson:encode({UriMap}).


malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).
