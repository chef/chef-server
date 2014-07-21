%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Douglas Triggs <doug@getchef.com>
%% @copyright 2014 Chef Software Inc

%% @doc System recovery endpoint

-module(oc_chef_wm_system_recovery).

-include_lib("chef_wm/include/chef_wm.hrl").
-include("oc_chef_wm.hrl").

-mixin([{chef_wm_base, [content_types_provided/2,
                        finish_request/2,
                        malformed_request/2,
                        ping/2 ]}]).

-mixin([{oc_chef_wm_base, [forbidden/2,
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
         process_post/2]).

init(Config) ->
  chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
  {ok, #user_state{}}.

request_type() ->
  "system_recovery".

allowed_methods(Req, State) ->
  {['POST'], Req, State}.

validate_request('POST', Req, #base_state{resource_state = UserState} = State) ->
  case wrq:req_body(Req) of
      Body  when Body == undefined orelse Body == <<>> ->
          throw({error, missing_body});
      Body ->
          UserData = ejson:decode(Body),
          case ej:valid(valid_user_data(), UserData) of
              ok ->
                  UserState1 = UserState#user_state{user_data = UserData},
                  {Req, State#base_state{superuser_bypasses_checks = true, resource_state = UserState1}};
              Bad ->
                  throw(Bad)
          end
  end.

valid_user_data() ->
    {[{<<"username">>, {string_match, chef_regex:regex_for(non_blank_string)}},
      {<<"password">>, {string_match, chef_regex:regex_for(non_blank_string)}}]}.

auth_info(Req, #base_state{ resource_state = #user_state{user_data = UserData}} = State) ->
    %% Disallow any attempts to authenticate with credentials of
    %% the platform superuser
    TargetName = chef_user:username_from_ejson(UserData),
    case oc_chef_wm_base:is_superuser(TargetName) of
        true ->
            {{halt, 403}, Req, State};
        _ ->
            %% But only permit the identified superuser
            %% to make this request (eg, only via
            %% webui)
            {superuser_only, Req, State}
    end.

process_post(Req, #base_state{chef_db_context = Ctx,
                              resource_state = #user_state{user_data = UserData}} = State) ->
    Name = chef_user:username_from_ejson(UserData),
    Password = ej:get({<<"password">>}, UserData),
    User = chef_db:fetch(#chef_user{username = Name}, Ctx),
    case verify_user(Password, User, Ctx) of
        {false, Code} ->
            {{halt, Code}, chef_wm_util:set_json_body(Req, auth_fail_message(Code)), State};
        EJson ->
            {true, chef_wm_util:set_json_body(Req, EJson), State}
    end.

verify_user(Password, #chef_user{recovery_authentication_enabled = true} = User, _Ctx) ->
    PasswordData = chef_user:password_data(User),
    case chef_password:verify(Password, PasswordData) of
        true ->
            user_json(User);
        false ->
            {false, 401}
    end;
verify_user(_Password, _Other, _Ctx) ->
    {false, 403}.

user_json(User) ->
    UserEJ0 = chef_user:assemble_user_ejson(User, undefined),
    UserEJ1 = ej:delete({<<"public_key">>}, UserEJ0),
    UserEJ2 = ej:set({<<"recovery_authentication_enabled">>}, UserEJ1, true),
    UserEJ2.

malformed_request_message({error, missing_body}, _Req, _State) ->
    chef_wm_util:error_message_envelope(<<"Invalid system recovery request">>);
malformed_request_message(#ej_invalid{}, _Req, _State) ->
    chef_wm_util:error_message_envelope(<<"Invalid system recovery request">>).

auth_fail_message(401) ->
    chef_wm_util:error_message_envelope(<<"Failed to authenticate: Username and password incorrect">>);
auth_fail_message(403) ->
    chef_wm_util:error_message_envelope(<<"System recovery disabled for this user">>);
auth_fail_message(502) ->
    chef_wm_util:error_message_envelope(<<"Authentication server is unavailable.">>).

