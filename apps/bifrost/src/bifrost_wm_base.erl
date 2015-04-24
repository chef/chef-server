-module(bifrost_wm_base).

-export([content_types_accepted/2,
         content_types_provided/2,
         create_path/2,
         finish_request/2,
         forbidden/2,
         init/2,
         malformed_request/2,
         ping/2,
         post_is_create/2,
         service_available/2,
         stats_hero_label/1,
         stats_hero_upstreams/0,
         validate_requestor/2]).

-include("bifrost_wm.hrl").
-include_lib("stats_hero/include/stats_hero.hrl").

init(Resource, Config) ->
    State = #base_state{module = Resource,
                        reqid = new_request_id(),
                        superuser_id = ?gv(superuser_id, Config),
                        request_type = ?gv(request_type, Config),
                        member_type = ?gv(member_type, Config),
                        metrics_config = ?gv(metrics_config, Config)},
    {ok, State}.

ping(Req, State) ->
    {pong, Req, State}.

service_available(Req, State) ->
    spawn_stats_hero_worker(Req, State),
    {true, Req, State}.

post_is_create(Req, State) ->
    {true, Req, State}.

malformed_request(Req, #base_state{module = Module} = State) ->
    % These following three may well come back as 'undefined' depending on the
    % requested endpoint, but that's fine, in those cases we don't care anyway since
    % we won't be using them in the first place:
    Id = wrq:path_info(id, Req),
    Action = case wrq:path_info(action, Req) of
                 undefined ->
                     undefined;
                 Permission ->
                     %% TODO: might want to revisit. Using list_to_atom
                     %% potentially opens us up to some trouble if a caller
                     %% tried many different perms (unlikely).
                     %% list_to_existing_atom fails on grant (where do we
                     %% populate that atom?).
                     list_to_atom(Permission)
             end,
    MemberId = wrq:path_info(member_id, Req),
    Module:validate_request(Req, State#base_state{authz_id = Id, action = Action,
                                                  member_id = MemberId}).

-spec validate_requestor(wm_req(), base_state()) ->
                                {{halt, 401 | 403 | 404}, wm_req(), base_state()} |
                                {boolean(), wm_req(), base_state()}.
validate_requestor(Req, State) ->
    try
        State0 = bifrost_wm_util:get_requestor(Req, State),
        case State0#base_state.requestor_id of
            undefined ->
                bifrost_wm_error:set_malformed_request(Req, State, missing_requestor);
            _ ->
                {false, Req, State0}
        end
    catch
        throw:{bad_requestor, Id} ->
            bifrost_wm_error:set_malformed_request(Req, State, {bad_requestor, Id})
    end.

forbidden(Req, #base_state{reqid = ReqId,
                           module = Module,
                           authz_id = Id,
                           request_type = Type,
                           requestor_id = RequestorId} = State) ->
    case Module:auth_info(wrq:method(Req)) of
        ignore ->
            {false, Req, State};
        Permission ->
            case bifrost_acl:check_access(ReqId, Type, Id, RequestorId,
                                           Permission) of
                true ->
                    {false, Req, State};
                false ->
                    bifrost_wm_error:set_access_exception(Req, State, Permission);
                {error, {invalid_target, _}} ->
                    {{halt, 404}, Req, State};
                {error, {invalid_actor, _}} ->
                    {{halt, 404}, Req, State}
            end
    end.

create_path(Req, State) ->
    AuthzId = bifrost_wm_util:generate_authz_id(),
    {AuthzId,
     %% Add new AuthzID to notes for output in the request logger
     wrq:add_note(created_authz_id, AuthzId, Req),
     State#base_state{authz_id = AuthzId}}.

content_types_accepted(Req, State) ->
    {[{"application/json", from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

finish_request(Req, #base_state{reqid=ReqId,
                                requestor_id=RequestorId,
                                module=Module}=State) ->
    Code = wrq:response_code(Req),
    %% Grab the stats for lager before we kill the stats_hero process
    PerfStats = stats_hero:snapshot(ReqId, no_agg),
    stats_hero:report_metrics(ReqId, Code),
    stats_hero:stop_worker(ReqId),

    %% Filter out extended perf data if necessary - this data approximately doubles
    %% size of access logs. Allowing this to be configured will allow for simple
    %% remediation if it becomes an issue.
    PerfStats1 = case envy:get(bifrost, enable_extended_perf_log, true, boolean) of
        true -> PerfStats;
        false ->
            [ Element || {<<"req_time">>,_} = Element <- PerfStats ]
    end,
    %% Add additional notes for the logger
    Req0 = oc_wm_request:add_notes([{reqid, ReqId},
                                    {requestor_id, RequestorId},
                                    {module, Module},
                                    {perf_stats, PerfStats1}], Req),
    {true, Req0, State}.

%% @doc Generates a new, unique request ID that we can use to attach
%% metrics to.  Taken from chef_wm, with the exception that we do not
%% currently offer the option to take a pre-generated request ID from
%% a header value.
%%
%% This generated ID is unique to Bifrost, and is distinct from the
%% requests to Erchef, Reporting, Pushy, etc. that ultimately trigger
%% this request.
-spec new_request_id() -> request_id().
new_request_id() ->
    base64:encode(crypto:hash(md5, term_to_binary(make_ref()))).


%% Stats Hero metrics-related functions
%%
%% Pretty simple for the time being; we only talk to a relational
%% database.  These functions keep the generated keys the same basic
%% "shape" as those coming from Erchef.

spawn_stats_hero_worker(Req, #base_state{reqid=ReqId,
                                         request_type=RequestType,
                                         module=Module,
                                         metrics_config=MetricsConfig}) ->
    %% Add the module and request type as a two-part label to allow us
    %% more fine-grained graphing capabilities.  Many modules handle
    %% multiple kinds of requests, so without this change, we end up
    %% obscuring quite a bit.
    RequestLabel = erlang:atom_to_list(Module) ++ "." ++ erlang:atom_to_list(RequestType),

    Config = [{request_id, ReqId},

              %% Bifrost doesn't have organizations
              {org_name, undefined},

              {my_app, ?gv(root_metric_key, MetricsConfig)},
              {request_action, atom_to_list(wrq:method(Req))},
              {protocol, envy:get(stats_hero, protocol, estatsd, atom)},
              {request_label, RequestLabel},
              {label_fun, ?gv(stats_hero_label_fun, MetricsConfig)},
              {upstream_prefixes, ?gv(stats_hero_upstreams, MetricsConfig)}
             ],
    case stats_hero_worker_sup:new_worker(Config) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            %% TODO: Need to put this to a separate file
            lager:error(io_lib:format("FAILED stats_hero_worker_sup:new_worker: ~p~n", [Reason])),
            ok
    end.

stats_hero_upstreams() ->
    [<<"rdbms">>].

stats_hero_label({bifrost_db, Fun}) ->
    %% DO NOT TAUNT HAPPY FUN BIN
    HappyFunBin = erlang:atom_to_binary(Fun, utf8),
    <<"rdbms.bifrost_db.", HappyFunBin/binary>>.
