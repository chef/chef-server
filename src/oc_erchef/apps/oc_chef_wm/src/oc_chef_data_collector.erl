-module(oc_chef_data_collector).

-include("oc_chef_wm.hrl").

-export([notify/2]).

-spec notify(Req :: wm_req(), State :: #base_state{}) -> ok.
notify(Req, #base_state{reqid = ReqId, resource_state = ResourceState, resource_mod = ResourceMod} = State) when
    is_record(ResourceState, acl_state);
    is_record(ResourceState, association_state);
    is_record(ResourceState, client_state);
    %% Skip controls because oc_chef_action can't extract them yet
    %% is_record(ResourceState, control_state);
    is_record(ResourceState, cookbook_state);
    is_record(ResourceState, data_state);
    is_record(ResourceState, environment_state);
    is_record(ResourceState, group_state);
    is_record(ResourceState, node_state);
    is_record(ResourceState, organization_state);
    is_record(ResourceState, role_state);
    is_record(ResourceState, user_state);
    is_record(ResourceState, key_state);
    is_record(ResourceState, policy_state);
    is_record(ResourceState, cookbook_artifact_version_state) ->
        case {wrq:method(Req), req_success(Req), ResourceMod} of
            %% Skip GET because we're not updating anything
            {'GET', _, _} ->
                ok;
            %% authenticate_user should not be an action.
            {_, _, oc_chef_wm_authenticate_user} ->
                ok;
            %% Notify any other 2XX message for matching resources
            {_, true, _} ->
                case stats_hero:ctime(ReqId, {data_collector, notify},
                                      fun() ->
                                              Msg = oc_chef_action:create_message(Req, State),
                                              data_collector_http:post("/", Msg)
                                      end) of
                    ok ->
                        ok;
                    {error, Error} ->
                        lager:debug("Data Collector notify failed: ~p", [Error])
                end;
            %% If we're here then we've matched a resource that we want to report
            %% but the request has to have failed with a non-successful response
            %% code (4XX or 5XX). Since we don't want to report errors to the data
            %% collector we'll skip the resource.
            {ReqMethod, _, _} ->
                ResCode = wrq:response_code(Req),
                lager:debug("Data Collector notify skipped for ~p ~p (~p)", [ReqMethod, ResourceState, ResCode]),
                ok
        end;

notify(_Req, _State) ->
    ok.

-spec req_success(Req :: wm_req()) -> boolean().
req_success(Req) ->
    case Code = integer_to_list(wrq:response_code(Req)) of
        Code = [$2|_] ->
            true;
        _ ->
            false
    end.
