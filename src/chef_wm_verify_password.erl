%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Mark Mzyk <mmzyk@opscode.com>
%% @copyright 2012 Opscode, Inc.

%% @doc Resource module for Chef users endpoint

-module(chef_wm_verify_password).

-include("chef_wm.hrl").

-mixin([{chef_wm_base, [content_types_accepted/2,
                        content_types_provided/2,
                        finish_request/2,
                        malformed_request/2,
                        ping/2 ]}]).

-mixin([{?BASE_RESOURCE, [ service_available/2 ]}]).

-behavior(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         request_type/0,
         validate_request/3]).

-export([allowed_methods/2,
         from_json/2,
         resource_exists/2,
         process_post/2]).

init(Config) ->
  chef_wm_base:init(?MODULE, Config).

%% Need to add the user_state
init_resource_state(_Config) ->
  {ok, #user_state{}}.

request_type() ->
  "users".

allowed_methods(Req, State) ->
    io:format("~nchef_wm_verify_password:allowed_methods()~n"),
  {['POST'], Req, State}.

validate_request('POST', Req, State) ->
  case wrq:req_body(Req) of
    undefined ->
      throw({error, missing_body});
   Body ->
      %% TODO: Placeholder
      UserData = ejson:decode(Body),
      {Req, State#base_state{resource_state =
          #user_state{user_data = UserData}}}
  end.

%% Create, destroy, and update are admin only actions
%% Need to update this to reflect that, as right now it
%% lets anyone through.
auth_info(Req, State) ->
  {authorized, Req, State}.

%% If we get here, are we guarenteed the user exists?
resource_exists(Req, State) ->
  {true, Req, State}.

from_json(Req, #base_state{reqid = RequestId,
                           resource_state = #user_state{user_data = UserData,
                           user_authz_id = AuthzId}} = State) ->
    Name = ej:get({<<"name">>}, UserData),
    Ejson = {[{<<"name">>, Name},
            {<<"verified">>, true}
        ]},
    {true, chef_wm_util:set_json_body(Req, Ejson), State}.

process_post(Req, #base_state{reqid = RequestId,
                           resource_state = #user_state{user_data = UserData,
                           user_authz_id = AuthzId}} = State) ->
    Name = ej:get({<<"name">>}, UserData),
    Ejson = {[{<<"name">>, Name},
            {<<"verified">>, true}
        ]},
    {true, chef_wm_util:set_json_body(Req, Ejson), State}.

