-module(oc_chef_authz_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("oc_chef_authz.hrl").
-include("oc_chef_types.hrl").

all() -> [fetch_container_sql].

init_per_suite(Config) ->
    ct:pal("hi from init~n"),
    suite_helper:init(Config).

end_per_suite(Config) ->
    ct:pal("bye from init~n"),
    suite_helper:stop(Config).

fetch_container_sql(_Config) ->
    OrgId = <<"33330000000000000000000000000000">>,
    ContainerName = <<"nodes">>,
    ReqId = <<"test-1-req-id">>,
    Ctx = oc_chef_authz:make_context(ReqId, darklaunch_stub),
    case oc_chef_authz_db:fetch_container_sql(Ctx, OrgId, ContainerName) of
        #chef_container{name = <<"nodes">>} = C ->
            ct:pal("Found container: ~p", [C]),
            ok;
        Bad ->
            erlang:error({unexpected_result, Bad})
    end.

