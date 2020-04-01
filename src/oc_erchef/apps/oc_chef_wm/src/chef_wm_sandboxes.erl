%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
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


-module(chef_wm_sandboxes).

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
         from_json/2 ]).

%% chef_wm behaviour callbacks
-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #sandbox_state{}}.

request_type() ->
    "sandboxes".

allowed_methods(Req, State) ->
    {['POST'], Req, State}.

-spec validate_request(chef_wm:http_verb(), wm_req(), chef_wm:base_state()) ->
                              {wm_req(), chef_wm:base_state()}.
validate_request('POST', Req, #base_state{resource_state = BoxState} = State) ->
    {ok, Sandbox} = chef_sandbox:parse_binary_json(wrq:req_body(Req), create),
    {Req, State#base_state{resource_state = BoxState#sandbox_state{sandbox_data = Sandbox}}}.

auth_info(Req, State) ->
    auth_info(wrq:method(Req), Req, State).

auth_info('POST', Req, State) ->
    {{create_in_container, sandbox}, Req, State}.

create_path(Req, #base_state{organization_guid = OrgId,
                             resource_state = SandboxState}=State) ->
    %% there is no name so to help with uniqueness, we take a digest of
    %% the content
    Name_ish = crypto:hash(md5, wrq:req_body(Req)),
    Id = chef_object_base:make_org_prefix_id(OrgId, Name_ish),
    SandboxState1 = SandboxState#sandbox_state{id = Id},
    {binary_to_list(Id), Req, State#base_state{resource_state = SandboxState1}}.

from_json(Req, #base_state{chef_db_context = DbContext,
                           requestor_id = ActorId,
                           organization_guid = OrgId,
                           resource_state =
                               #sandbox_state{sandbox_data = SandboxData}} = State) ->
    Checksums = checksums_from_sandbox_ejson(SandboxData),
io:format("~n~nIN CHEF_WM_SANDBOXES.ERL~nChecksums = ~p~nSandboxData = ~p~n ", [Checksums, SandboxData]),
    case chef_db:make_sandbox(DbContext, OrgId, ActorId, Checksums) of
        #chef_sandbox{} = Sandbox ->
            Response = sandbox_to_response(Req, Sandbox),
            {true, chef_wm_util:set_json_body(Req, Response), State};
        {conflict, _Msg} ->
            {{halt, 409}, Req, State};
        {error, _EWhy} ->
            {{halt, 500}, Req, State}
    end.

malformed_request_message(#ej_invalid{type=fun_match, key=Key}, _Req, _State)
  when Key =:= <<"checksums">> ->
    {[{<<"error">>, [<<"Field '", Key/binary, "' invalid">>]}]};
malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

checksums_from_sandbox_ejson(SandboxData) ->
    {ChecksumList} = ej:get({<<"checksums">>}, SandboxData),
    [ CSum || {CSum, null} <- ChecksumList ].

sandbox_to_response(Req, #chef_sandbox{id = Id, org_id = OrgId, checksums = ChecksumList}) ->
    Ans = {[{<<"sandbox_id">>, Id},
            {<<"uri">>, oc_chef_wm_routes:route(sandbox, Req, [{id, Id}])},
            {<<"checksums">>,
             {
               [ {CSum, checksum_data(CSum, Flag, OrgId, chef_wm_util:base_uri(Req))} || {CSum, Flag} <- ChecksumList ]
             }
            }
           ]},
    Ans.

-define(PUT_URL_TTL, 900).

checksum_data(_CSum, true, _OrgId, _VHost) ->
    {[
      {<<"needs_upload">>, false}
     ]};
checksum_data(CSum, false, OrgId, VHost) ->
    %% FIXME: need to do a lookup either one at a time or in bulk to find out which
    %% checksums are already in the db.
    %% FIXME: will this be a problem for OSC w/ bookshelf?
    PutUrl = chef_s3:generate_presigned_url(OrgId, ?PUT_URL_TTL, put, CSum, VHost),
    {[
      {<<"url">>, PutUrl},
      {<<"needs_upload">>, true}
     ]}.
