%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Mark Mzyk <mmzyk@opscode.com>
%% @copyright 2012 Opscode, Inc.

%% @doc Resource module for Chef users endpoint

-module(chef_wm_authenticate_user).

-include("chef_wm.hrl").

-mixin([{chef_wm_base, [content_types_provided/2,
                        finish_request/2,
                        malformed_request/2,
                        ping/2 ]}]).

-mixin([{?BASE_RESOURCE, [ service_available/2 ]}]).

-behavior(chef_wm).
-export([
         auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3]).

-export([allowed_methods/2,
         process_post/2]).

init(Config) ->
  chef_wm_base:init(?MODULE, Config).

%% Need to add the user_state
init_resource_state(_Config) ->
  {ok, #user_state{}}.

request_type() ->
  "users".

allowed_methods(Req, State) ->
  {['POST'], Req, State}.

validate_request('POST', Req, #base_state{resource_state = UserState} = State) ->
  case wrq:req_body(Req) of
      undefined ->
          throw({error, missing_body});
      Body ->
          UserData = ejson:decode(Body),
          case ej:valid(valid_user_data(), UserData) of
              ok ->
                  UserState1 = UserState#user_state{user_data = UserData},
                  {Req, State#base_state{resource_state = UserState1}};
              Bad ->
                  throw(Bad)
          end
  end.

valid_user_data() ->
    {[{<<"name">>, string},
      {<<"password">>, string}]}.

%% Create, destroy, and update are admin only actions
%% Need to update this to reflect that, as right now it
%% lets anyone through.
auth_info(Req, State) ->
  {authorized, Req, State}.

process_post(Req, #base_state{chef_db_context = DbContext,
                              resource_state =
                                  #user_state{user_data = UserData}} = State) ->
    Name = ej:get({<<"name">>}, UserData),
    Verified = case chef_db:fetch_user(DbContext, Name) of
                   not_found ->
                       false;
                   #chef_user{hashed_password = HashedPass,
                              salt = Salt,
                              hash_type = HashType} ->
                       Password = ej:get({<<"password">>}, UserData),
                       chef_wm_password:verify(Password, {HashedPass, Salt, HashType})
               end,
    Ejson = {[{<<"name">>, Name},
              {<<"verified">>, Verified}
             ]},
    {true, chef_wm_util:set_json_body(Req, Ejson), State}.

malformed_request_message(#ej_invalid{}, _Req, _State) ->
    Msg = <<"invalid user authentication request">>,
    chef_wm_util:error_message_envelope(Msg).
