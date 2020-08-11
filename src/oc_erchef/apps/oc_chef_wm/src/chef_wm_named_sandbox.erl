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


-module(chef_wm_named_sandbox).


-include("oc_chef_wm.hrl").

%% Webmachine resource callbacks
-mixin([{oc_chef_wm_base, [content_types_accepted/2,
                           content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           forbidden/2,
                           is_authorized/2,
                           service_available/2]}]).

-export([allowed_methods/2,
         from_json/2,
         resource_exists/2]).

%% chef_wm behavior callbacks
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
    {['PUT'], Req, State}.

auth_info(Req, State) ->
    % POST /sandboxes and PUT /sandboxes/<id> are two halves of a single upload
    % operation, so we use the same permission for both (create sandbox perm)
    {[{container, sandbox, create}], Req, State}.

%% Org is checked for in malformed_request/2, sandbox is checked for in forbidden/2;
%% if we get this far, it exists.
resource_exists(Req, #base_state{chef_db_context = DbContext,
                                 organization_guid = OrgId,
                                 resource_state = SandboxState} = State) ->
    SandboxId = chef_wm_util:object_name(sandbox, Req),
    case chef_db:fetch(#chef_sandbox{org_id = OrgId, id = SandboxId}, DbContext) of
        not_found ->
            Message = chef_wm_util:not_found_message(sandbox, SandboxId),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            %% we want a 404 on PUT
            {{halt, 404}, Req1, State#base_state{log_msg = sandbox_not_found}};
        Sandbox = #chef_sandbox{} ->
            SandboxState1 = SandboxState#sandbox_state{chef_sandbox = Sandbox},
            {true, Req, State#base_state{resource_state = SandboxState1}};
        {error, Why} ->
            {{halt, 500}, Req, State#base_state{log_msg = Why}}
    end.

%% For back-compatibility, we return a "sandbox" response when a client commits a sandbox. A
%% simple 204 would be more appropriate. The client already has all of the data we will send
%% and the sandbox is deleted as part of the commit action, so there is nothing the client
%% can do with a sandbox after committing it. Chef 0.10.z clients crash on 204 responses
%% (code in chef/rest.rb assumes a non-nil body). For a small cleanup, we've removed the
%% json_class field since even Chef 0.10.z clients do not operate on the returned data.
from_json(Req, #base_state{reqid = ReqId,
                           chef_db_context = DbContext,
                           resource_state = #sandbox_state{chef_sandbox = Sandbox}} = State) ->
    try
        %% ReqId needed here for chef_s3 instrumentation
        validate_checksums_uploaded(ReqId, Sandbox),
        commit_sandbox(DbContext, Sandbox),
        Req1 = chef_wm_util:set_json_body(Req, sandbox_to_ejson(Sandbox)),
        {true, Req1, State}
    catch
        throw:{checksum_check_error, Errors} ->
            % i am assuming here (need confirmation) that a 404 on committing an incomplete
            % sandbox means we should return a 503 (service unavailable), which is what the
            % pedant test wants.
            Msg = iolist_to_binary([<<"Committed incomplete sandbox: ">>, io_lib:format("~p", [Errors])]),
            EMsg = chef_wm_util:error_message_envelope(Msg),
            {{halt, 503}, chef_wm_util:set_json_body(Req, EMsg), State};

        throw:{missing_checksums, Sums} ->
            SumList0 = iolist_to_binary([ <<(as_binary(S))/binary, ", ">> || S <- Sums]),
            SumList = binary:part(SumList0, {0, size(SumList0) - 2}),
            Msg = iolist_to_binary([<<"Cannot update sandbox ">>, Sandbox#chef_sandbox.id,
                                    <<": the following checksums have not been uploaded: ">>,
                                    SumList]),
            EMsg = chef_wm_util:error_message_envelope(Msg),
            {{halt, 503}, chef_wm_util:set_json_body(Req, EMsg), State}
    end.

%% Private utility functions
malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

-spec validate_request(chef_wm:http_verb(), wm_req(), chef_wm:base_state()) ->
                              {wm_req(), chef_wm:base_state()}.
validate_request('PUT', Req, State) ->
    %% it's OK if this throws, will be handled by caller of validate_request/2.
    Body = chef_json:decode(wrq:req_body(Req)),
    case ej:get({<<"is_completed">>}, Body) of
        true ->
            {Req, State#base_state{resource_state = #sandbox_state{}}};
        _ ->
            Msg = <<"JSON body must contain key '\"complete\"' with value 'true'.">>,
            throw({invalid, Msg})
    end.

validate_checksums_uploaded(ReqId, #chef_sandbox{id = _BoxId, checksums = Checksums, org_id = OrgId}) ->
    %% The flag is for "uploaded"
    NeedsUpload = [ CSum || {CSum, false} <- Checksums ],

    {{ok, _}, {missing, NotFound}, {timeout, Timeouts}, {error, Errors}}  =
        ?SH_TIME(ReqId, chef_s3, check_checksums, (OrgId, NeedsUpload)),
    %% For the time being, we are lumping timeouts in with other errors
    OverallErrorCount = length(Errors) + length(Timeouts),

    case {NotFound, OverallErrorCount} of
        {[], 0} ->
            %% Everything was there!
            ok;
        {_, OverallErrorCount} when OverallErrorCount =/= 0 ->
            %% We had some errors :(
            %% gets a 404 when committing an incomplete sandbox
            %% 404 winds up here. don't know if other errors
            %% wind up here as well.
            throw({checksum_check_error, Errors});
        {Missing, _} ->
            %% Some checksums were missing :(
            throw({missing_checksums, Missing})
    end.

%% @doc Commits a sandbox.  This marks all checksums as having been uploaded, and removes
%% all record of the sandbox from the database (it has served its purpose and is no longer
%% of any use).
commit_sandbox(Ctx, #chef_sandbox{org_id = OrgId, checksums = Checksums}=Sandbox) ->
    %% mark_checksums_as_uploaded creates checksum records in the database for
    %% the org_id, checksum combination. The checksums list in a #chef_sandbox
    %% record is a list of {checksum, is_uploaded}, where is_uploaded is derived from
    %% the existence of the org_id, checksum row in the checksums table. Therefore,
    %% we do not need to mark a checksum as uploaded if is_uploaded is true. This will
    %% prevent us from generating conflicts and error messages on the database.
    ChecksumsToCommit = [ C || {C, false} <- Checksums ],
    chef_db:mark_checksums_as_uploaded(Ctx, OrgId, ChecksumsToCommit),
    chef_db:commit_sandbox(Ctx, Sandbox).

sandbox_to_ejson(#chef_sandbox{id = Id, checksums = Checksums}) ->
    %% TODO: use the timestamp from the record created_at. Avoiding that for now because we
    %% may have some conversion issues in our config to deal with.
    When = timestamp(now),
    {[
      {<<"guid">>, Id},
      {<<"name">>, Id},
      {<<"checksums">>, [ C || {C, _} <- Checksums ]},
      {<<"create_time">>, When},
      {<<"is_completed">>, true}
     ]}.

as_binary(S) when is_list(S) ->
    list_to_binary(S);
as_binary(B) when is_binary(B) ->
    B.


timestamp(now) ->
    timestamp(os:timestamp());
timestamp({_,_,_} = TS) ->
    {{Year,Month,Day},{Hour,Minute,Second}} = calendar:now_to_universal_time(TS),
    iolist_to_binary(io_lib:format("~4w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w+00:00",
                  [Year, Month, Day, Hour, Minute, Second])).
