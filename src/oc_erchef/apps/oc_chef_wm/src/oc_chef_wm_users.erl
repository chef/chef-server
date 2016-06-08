%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Mark Mzyk <mmzyk@chef.io>
%% @author Marc Paradise <marc@chef.io>
%% @copyright 2012-2015 Chef Software, Inc.

%% @doc Resource module for Chef users endpoint

-module(oc_chef_wm_users).


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
         create_path/2,
         from_json/2,
         resource_exists/2,
         to_json/2]).

%% chef_wm behavior callbacks
-behavior(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         conflict_message/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3,
         finalize_create_body/4 ]).


init(Config) ->
  oc_chef_wm_base:init(?MODULE, Config).

%% Need to add the user_state
init_resource_state(_Config) ->
  {ok, #user_state{}}.

request_type() ->
  "users".

allowed_methods(Req, State) ->
  {['GET', 'POST'], Req, State}.

-spec validate_request(chef_wm:http_verb(), wm_req(), chef_wm:base_state()) ->
                              {wm_req(), chef_wm:base_state()}.
validate_request('POST', Req, #base_state{server_api_version = ApiVersion} = State) ->
  case wrq:req_body(Req) of
    undefined ->
      throw({error, missing_body});
   Body ->
      {ok, UserData} = chef_user:parse_binary_json(ApiVersion, Body),
      {Req, State#base_state{resource_state =
          #user_state{user_data = UserData}}}
  end;
validate_request('GET', Req, State) ->
    {Req, State}.

-spec auth_info(wm_req(), chef_wm:base_state()) ->
                       chef_wm:auth_info_return().
auth_info(Req, State) ->
    auth_info(wrq:method(Req), Req, State).

-spec auth_info(chef_wm:http_verb(), wm_req(), chef_wm:base_state()) ->
                       chef_wm:auth_info_return().
auth_info('GET', Req, State) ->
    {{container, user}, Req, State};
auth_info('POST', Req, State) ->
    {{create_in_container, user}, Req, State}.

resource_exists(Req, State) ->
  {true, Req, State}.

create_path(Req, #base_state{resource_state = #user_state{user_data = UserData}} = State) ->
  Name = chef_user:username_from_ejson(UserData),
  {binary_to_list(Name), Req, State}.

from_json(Req, #base_state{resource_state = #user_state{user_data = UserData, user_authz_id = AuthzId}} = State) ->
    KeyContext = #key_context{ object_name = chef_user:username_from_ejson(UserData),
                               object_authz_id = AuthzId,
                               object_ej = UserData,
                               type = user},
    oc_chef_wm_key_base:create_object_with_embedded_key_data(Req, State#base_state{key_context = KeyContext}).

% Callback from create_from_json, which allows us to customize our body response.
%
finalize_create_body(_Req, #base_state{server_api_version = ?API_v0,
                                       key_context = #key_context{key_ej = KeyEJ}}, _User,  BodyEJ ) ->
    case ej:get({<<"private_key">>}, KeyEJ) of
        undefined ->
            BodyEJ;
        PrivateKey ->
            ej:set({<<"private_key">>}, BodyEJ, PrivateKey)
    end;
finalize_create_body(Req, State, _Client, BodyEJ) ->
    oc_chef_wm_key_base:finalize_create_body(Req, State, BodyEJ).

to_json(Req, State) ->
    %% In the case of verbose, we cannot  use standard chef_wm_base behavior -
    %% the client expects the fields email, first_name, last_name - while
    %% the standard response tries to give us a URI.
    %% Secondary note: the original interface in opscode-account supported the combination of
    %% both email filter and verbose option, but this was unused
    %% and it adds further complication. It is not supported here now.
    case wrq:get_qs_value("verbose", Req) of
        "true" ->
            {chef_json:encode(verbose_users_as_ejson()), Req, State};
        _ ->
            oc_chef_wm_base:list_objects_json(Req, State#base_state{resource_state =
                                                                        #chef_user{email =
                                                                                       wrq:get_qs_value("email", Req),
                                                                                   external_authentication_uid =
                                                                                       wrq:get_qs_value("external_authentication_uid", Req)}
                                                                   })
    end.

verbose_users_as_ejson() ->
    case sqerl:select(list_users_verbose, [], {rows_as_records, [chef_user, record_info(fields, chef_user)]}) of
        {ok, none} ->
            {[{}]};
        {ok, Records} ->
            {[ verbose_user(User) || User  <- Records]}
    end.

verbose_user(#chef_user{username = UserName, email = EMail, serialized_object = SerializedObject }) ->
    EJ = chef_json:decode(SerializedObject),
    {UserName, {[ {<<"email">>, EMail},
                  {<<"first_name">>, ej:get({<<"first_name">>}, EJ, "")},
                  {<<"last_name">>, ej:get({<<"last_name">>}, EJ, "")} ]} }.



conflict_message(Name) ->
    Msg = iolist_to_binary([<<"User '">>, Name, <<"' already exists">>]),
    {[{<<"error">>, [Msg]}]}.

malformed_request_message(Any, _Req, _state) ->
    error({unexpected_malformed_request_message, Any}).
