%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% @copyright 2012 Opscode, Inc.

-module(chef_wm_sandboxes).

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

-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3]).

-export([
         allowed_methods/2,
         create_path/2,
         from_json/2,
         resource_exists/2
        ]).

init(Config) ->
    chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #sandbox_state{}}.

request_type() ->
    "sandboxes".

allowed_methods(Req, State) ->
    {['POST'], Req, State}.

validate_request('POST', Req, #base_state{resource_state = BoxState} = State) ->
    {ok, Sandbox} = chef_sandbox:parse_binary_json(wrq:req_body(Req), create),
    {Req, State#base_state{resource_state = BoxState#sandbox_state{sandbox_data = Sandbox}}}.

auth_info(Req, State) ->
    auth_info(wrq:method(Req), Req, State).

auth_info('POST', Req, State) ->
    {{create_in_container, sandbox}, Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

create_path(Req, #base_state{organization_guid = OrgId,
                             resource_state = SandboxState}=State) ->
    %% there is no name, to help with uniqueness, maybe take first checksum or digest of content?
    Name_ish = crypto:md5(wrq:req_body(Req)),
    Id = chef_object:make_org_prefix_id(OrgId, Name_ish),
    SandboxState1 = SandboxState#sandbox_state{id = Id},
    {binary_to_list(Id), Req, State#base_state{resource_state = SandboxState1}}.

from_json(Req, #base_state{chef_db_context = DbContext,
                           requestor_id = ActorId,
                           organization_name = OrgName,
                           resource_state =
                               #sandbox_state{sandbox_data = SandboxData}} = State) ->
    Checksums = checksums_from_sandbox_ejson(SandboxData),
    case chef_db:make_sandbox(DbContext, OrgName, ActorId, Checksums) of
        #chef_sandbox{} = Sandbox ->
            Response = sandbox_to_response(Req, Sandbox),
            {true, chef_wm_util:set_json_body(Req, Response), State};
        {conflict, _Msg} ->
            {{halt, 409}, Req, State};
        {error, _EWhy} ->
            {{halt, 500}, Req, State}
    end.

malformed_request_message({empty_checksums, Msg}, _Req, _State) ->
    chef_wm_util:error_message_envelope(Msg);
malformed_request_message({bad_checksum, Msg}, _Req, _State) ->
    chef_wm_util:error_message_envelope(Msg);
malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

checksums_from_sandbox_ejson(SandboxData) ->
    {ChecksumList} = ej:get({<<"checksums">>}, SandboxData),
    [ CSum || {CSum, null} <- ChecksumList ].


sandbox_to_response(Req, #chef_sandbox{id = Id, org_id = OrgId, checksums = ChecksumList}) ->
    Ans = {[{<<"sandbox_id">>, Id},
            {<<"uri">>, ?BASE_ROUTES:route(sandbox, Req, [{id, Id}])},
            {<<"checksums">>,
             {
               [ {CSum, checksum_data(CSum, Flag, OrgId)} || {CSum, Flag} <- ChecksumList ]
             }
            }
           ]},
    Ans.

-define(PUT_URL_TTL, 900).

checksum_data(_CSum, true, _OrgId) ->
    {[
      {<<"needs_upload">>, false}
     ]};
checksum_data(CSum, false, OrgId) ->
    %% FIXME: need to do a lookup either one at a time or in bulk to find out which
    %% checksums are already in the db.
    %% FIXME: will this be a problem for OSC w/ bookshelf?
    PutUrl = chef_s3:generate_presigned_url(OrgId, ?PUT_URL_TTL, put, CSum),
    {[
      {<<"url">>, PutUrl},
      {<<"needs_upload">>, true}
     ]}.
