%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@chef.io>
%% @author James Casey <james@chef.io>
%% @author Mark Mzyk <mmzyk@chef.io>
%% Copyright 2012-2018 Chef Software, Inc.
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


-module(chef_wm_cookbook_version).

-include("oc_chef_wm.hrl").

-mixin([{oc_chef_wm_base, [content_types_accepted/2,
                          content_types_provided/2,
                          finish_request/2,
                          malformed_request/2,
                          ping/2,
                          forbidden/2,
                          is_authorized/2,
                          service_available/2]}]).

%% chef_wm behavior callbacks
-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3]).

-export([allowed_methods/2,
         delete_resource/2,
         from_json/2,
         conflict_message/1,
         is_conflict/2,
         to_json/2]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #cookbook_state{}}.

request_type() ->
    "cookbooks".

allowed_methods(Req, State) ->
    {['GET', 'PUT', 'DELETE'], Req, State}.

-spec validate_request(chef_wm:http_verb(), wm_req(), chef_wm:base_state()) ->
                              {wm_req(), chef_wm:base_state()}.
validate_request(Method, Req, #base_state{server_api_version = ApiVersion,
                                          resource_state = CBState0} = State) ->
    UrlName = chef_wm_util:extract_from_path(cookbook_name, Req),
    UrlVersion = chef_wm_util:extract_from_path(cookbook_version, Req),
    Version = parse_cookbook_version(Method, UrlVersion),
    CBState1 = CBState0#cookbook_state{cookbook_name = UrlName,
                                       cookbook_version = Version},
    CBState = case Method of
                'PUT' ->
                  Body = wrq:req_body(Req),
                  {ok, Cookbook} = chef_cookbook_version:parse_binary_json(Body, {UrlName, UrlVersion}, chef_cookbook_version:wants_all_files(ApiVersion)),
                  CBState1#cookbook_state{cookbook_data = Cookbook};
                _ ->
                  CBState1
                end,
    {Req, State#base_state{resource_state = CBState}}.

auth_info(Req, #base_state{chef_db_context = DbContext,
                           organization_guid = OrgId,
                           resource_state = #cookbook_state{cookbook_name = Name,
                                                            cookbook_version = Version} = CookbookState
                          } = State) ->
    case fetch_cookbook_version(DbContext, OrgId, Name, Version) of
        not_found ->
            handle_not_found(Req, State);
        {cookbook_exists, AuthzId} ->
            CookbookState1 = CookbookState#cookbook_state{authz_id = AuthzId},
            handle_cookbook_exists(Req, State#base_state{resource_state = CookbookState1});
        #chef_cookbook_version{authz_id = AuthzId} = CookbookVersion ->
            CookbookState1 = CookbookState#cookbook_state{authz_id = AuthzId,
                                                          chef_cookbook_version = CookbookVersion},
            State1 = State#base_state{resource_state = CookbookState1},
            {{object, AuthzId}, Req, State1}
    end.

is_conflict(Req, #base_state{}=State) ->
    handle_is_conflict(wrq:method(Req), Req, State).

handle_is_conflict('PUT', Req, #base_state{resource_state = CookbookState} = State) ->
    CookbookVersion = CookbookState#cookbook_state.chef_cookbook_version,
    case can_update(Req, CookbookVersion) of
        true ->
            {false, Req, State};
        false ->
            LogMsg = {chef_cookbook_version, frozen, CookbookVersion#chef_cookbook_version.id},
            ConflictMsg = conflict_message(CookbookVersion),
            {true, chef_wm_util:set_json_body(Req, ConflictMsg),
             State#base_state{log_msg = LogMsg}}
    end;
handle_is_conflict(_, Req, State) ->
    {false, Req, State}.

fetch_cookbook_version(DbContext, OrgId, Name, latest) ->
  chef_db:fetch_latest_cookbook_version(DbContext, OrgId, Name);
fetch_cookbook_version(DbContext, OrgId, Name, Version) ->
  chef_db:fetch_cookbook_version(DbContext, OrgId, {Name, Version}).

to_json(Req, #base_state{server_api_version = Version,
                         resource_state=#cookbook_state{chef_cookbook_version=CBV}}=State) ->
    CompleteEJson = chef_cookbook_version:assemble_cookbook_ejson(CBV, chef_wm_util:base_uri(Req), chef_cookbook_version:wants_all_files(Version)),
    {chef_json:encode(CompleteEJson), Req, State}.

from_json(Req, #base_state{resource_state = CookbookState} = State) ->
    case cookbook_version_exists(CookbookState) of
        true ->
            #cookbook_state{cookbook_data = CBData,
                            chef_cookbook_version = CBVersion} = CookbookState,
            %% The "frozen?" attribute is immutable once it has been set to true for a given
            %% CBV. Since the update_from_json helper function sets the reponse body to the
            %% JSON passed in, we do the fixup of the attribute here. The rule is: if old
            %% version has frozen? true, then stay true, otherwise take the value from the
            %% request and default to false.
            Frozen = (CBVersion#chef_cookbook_version.frozen =:= true orelse
                      ej:get({<<"frozen?">>}, CBData, false)),
            CBData1 = ej:set({<<"frozen?">>}, CBData, Frozen),
            oc_chef_wm_base:update_from_json(Req, State, CBVersion, CBData1);
        false ->
            #cookbook_state{authz_id = AuthzId,
                            cookbook_data = CBData} = CookbookState,
            case oc_chef_wm_base:create_from_json(Req, State, chef_cookbook_version,
                                               {authz_id, AuthzId}, CBData) of
                {true, Req1, State1} ->
                    %% munge response since we are using PUT as create here
                    {true, chef_wm_util:set_json_body(Req1, CBData), State1};
                {_, Req2, #base_state{log_msg = {error, invalid_checksum}} = State2} ->
                    Msg = <<"Manifest has a checksum that hasn't been uploaded.">>,
                    JsonError = {[{<<"error">>, [Msg]}]},
                    {{halt, 400}, chef_wm_util:set_json_body(Req2, JsonError), State2};
                Else ->
                    Else
            end
    end.

delete_resource(Req, #base_state{chef_db_context = DbContext,
                                 requestor_id = RequestorId,
                                 resource_state = #cookbook_state{
                                                     chef_cookbook_version = CookbookVersion}
                                } = State) ->
    ok = oc_chef_wm_base:delete_object(DbContext, CookbookVersion, RequestorId),
    Json = chef_cookbook_version:assemble_cookbook_ejson(CookbookVersion, chef_wm_util:base_uri(Req)),
    {true, chef_wm_util:set_json_body(Req, Json), State}.

%% Private utility functions

cookbook_version_exists(#cookbook_state{chef_cookbook_version = undefined}) ->
    false;
cookbook_version_exists(#cookbook_state{chef_cookbook_version = #chef_cookbook_version{} }) ->
    true.

construct_not_found_response(Req, #base_state{resource_state = #cookbook_state{cookbook_name = Name}} = State) ->
    Message = chef_wm_util:not_found_message(cookbook_version, {Name, chef_wm_util:extract_from_path(cookbook_version, Req)}),
    {{halt, 404},
     chef_wm_util:set_json_body(Req, Message),
     State#base_state{log_msg = cookbook_version_not_found}}.

%% helper function for auth_info/2 to handle the case when no versions of the cookbook exist
handle_not_found(Req, #base_state{} = State) ->
    case wrq:method(Req) of
        'PUT' ->
            {{create_in_container, cookbook}, Req, State};
        _ ->
            construct_not_found_response(Req, State)
    end.

%% helper function for forbidden/2 to handle the case when the cookbook exists but the
%% requested cookbook version does not exist case
handle_cookbook_exists(Req, #base_state{resource_state = #cookbook_state{authz_id = AuthzId}} = State) ->
    case wrq:method(Req) of
        'PUT' ->
            {{object, AuthzId}, Req, State};
        _ ->
            construct_not_found_response(Req, State)
    end.

malformed_request_message(#ej_invalid{type = array_elt,
                                      key = Key},
                          _Req, _State) ->
    error_message([<<"Invalid element in array value of '">>, Key, <<"'.">>]);


malformed_request_message({bad_cookbook_name, Name, Pattern}, _Req, _State) ->
    Msg = [<<"Invalid cookbook name '">>, Name, <<"' using regex: '">>, Pattern, <<"'.">>],
    error_message(Msg);
malformed_request_message({bad_cookbook_version, Version}, _Req, _State) ->
    Msg = [<<"Invalid cookbook version '">>, Version, <<"'.">>],
    error_message(Msg);
malformed_request_message(fixme_all_cookbooks_bad, _Req, _State) ->
    Msg = <<"FIXME: all cookbooks are 400 right now">>,
    {[{<<"error">>, [Msg]}]};
malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).


parse_cookbook_version(Method, UrlVersion) when Method =:= 'GET';
                                                Method =:= 'DELETE' ->
  case UrlVersion of
    Special when Special =:= <<"_latest">>;
                 Special =:= <<"latest">> ->
     latest;
    _Version ->
      parse_cookbook_version(UrlVersion)
  end;
parse_cookbook_version(_, UrlVersion) ->
  parse_cookbook_version(UrlVersion).

parse_cookbook_version(UrlVersion) ->
  try chef_cookbook_version:parse_version(UrlVersion) of
    {_Major, _Minor, _Patch} = Version ->
     Version
    catch
      error:badarg ->
        throw({bad_cookbook_version, UrlVersion})
  end.

error_message(Msg) when is_list(Msg) ->
    error_message(iolist_to_binary(Msg));
error_message(Msg) when is_binary(Msg) ->
    {[{<<"error">>, [Msg]}]}.

conflict_message(#chef_cookbook_version{name = Name,
                                        major = Major, minor = Minor, patch = Patch}) ->
    Msg = [<<"The cookbook ">>, Name, <<" at version ">>,
           chef_cookbook_version:version_to_binary({Major, Minor, Patch}),
           <<" is frozen. Use the 'force' option to override.">>],
    {[{<<"error">>, [iolist_to_binary(Msg)]}]};
conflict_message(_Name) ->
    {[{<<"error">>, [<<"Cookbook already exists">>]}]}.

-spec is_forced(#wm_reqdata{}) ->  true | false.
is_forced(Req) ->
    case wrq:get_qs_value("force", Req) of
        Force when Force =:= undefined;
                   Force =:= "false" ->
            false;
        _ ->
            %% any value other than "false" is true.
            true
    end.

-spec can_update(#wm_reqdata{}, #chef_cookbook_version{}) ->  true | false.
can_update(Req, #chef_cookbook_version{frozen = Frozen}) ->
    is_forced(Req) orelse Frozen =:= false.
