-module(chef_license_worker_test).

-include_lib("eunit/include/eunit.hrl").

-export([]).

-define(DEFAULT_FILE_PATH, "/tmp/lic").

-define(DEFAULT_CONFIG, []).

get_commercial_license()->
    <<"{\"command\":\"chef-automate license status --result-json /tmp/string3\",\"status\":\"OK\",\"error_code\":0,\"error_description\":\"\",\"error_cause\":\"\",\"error_stack_trace\":\"\",\"error_recovery\":\"\",\"error_type\":\"\",\"result\":{\"set\":true,\"license_id\":\"6541d90a-2ed0-4d64-9861-c20fc21a3093\",\"customer_name\":\"janshahid.shaik@progress.com\",\"expiration_date\":{\"seconds\":1835689599},\"deployment_id\":\"fa7cce37-d8b3-4542-ad38-2e587a05faec\",\"deployment_type\":\"Standalone\",\"license_type\":\"commercial\",\"deployment_at\":{\"seconds\":1727169083}}}">>.

get_commercial_license_expired()->
    <<"{\"command\":\"chef-automate license status --result-json /tmp/string3\",\"status\":\"OK\",\"error_code\":0,\"error_description\":\"\",\"error_cause\":\"\",\"error_stack_trace\":\"\",\"error_recovery\":\"\",\"error_type\":\"\",\"result\":{\"set\":true,\"license_id\":\"6541d90a-2ed0-4d64-9861-c20fc21a3093\",\"customer_name\":\"janshahid.shaik@progress.com\",\"expiration_date\":{\"seconds\":1635689599},\"deployment_id\":\"fa7cce37-d8b3-4542-ad38-2e587a05faec\",\"deployment_type\":\"Standalone\",\"license_type\":\"commercial\",\"deployment_at\":{\"seconds\":1727169083}}}">>.

get_commercial_grace_license()->
    <<"{\"command\":\"chef-automate license status --result-json /tmp/string3\",\"status\":\"OK\",\"error_code\":0,\"error_description\":\"\",\"error_cause\":\"\",\"error_stack_trace\":\"\",\"error_recovery\":\"\",\"error_type\":\"\",\"result\":{\"set\":true,\"grace_period\":true,\"license_id\":\"6541d90a-2ed0-4d64-9861-c20fc21a3093\",\"customer_name\":\"janshahid.shaik@progress.com\",\"expiration_date\":{\"seconds\":1635689599},\"deployment_id\":\"fa7cce37-d8b3-4542-ad38-2e587a05faec\",\"deployment_type\":\"Standalone\",\"license_type\":\"commercial\",\"deployment_at\":{\"seconds\":1727169083}}}">>.

get_trail_license()->
    <<"{\"command\":\"chef-automate license status --result-json /tmp/string3\",\"status\":\"OK\",\"error_code\":0,\"error_description\":\"\",\"error_cause\":\"\",\"error_stack_trace\":\"\",\"error_recovery\":\"\",\"error_type\":\"\",\"result\":{\"set\":true,\"grace_period\":true,\"license_id\":\"6541d90a-2ed0-4d64-9861-c20fc21a3093\",\"customer_name\":\"janshahid.shaik@progress.com\",\"expiration_date\":{\"seconds\":1835689599},\"deployment_id\":\"fa7cce37-d8b3-4542-ad38-2e587a05faec\",\"deployment_type\":\"Standalone\",\"license_type\":\"trail\",\"deployment_at\":{\"seconds\":1727169083}}}">>.

get_trail_license_expired()->
    <<"{\"command\":\"chef-automate license status --result-json /tmp/string3\",\"status\":\"OK\",\"error_code\":0,\"error_description\":\"\",\"error_cause\":\"\",\"error_stack_trace\":\"\",\"error_recovery\":\"\",\"error_type\":\"\",\"result\":{\"set\":true,\"grace_period\":true,\"license_id\":\"6541d90a-2ed0-4d64-9861-c20fc21a3093\",\"customer_name\":\"janshahid.shaik@progress.com\",\"expiration_date\":{\"seconds\":1635689599},\"deployment_id\":\"fa7cce37-d8b3-4542-ad38-2e587a05faec\",\"deployment_type\":\"Standalone\",\"license_type\":\"trail\",\"deployment_at\":{\"seconds\":1727169083}}}">>.

license_test()->
    application:start(chef_license),
    file:write_file(?DEFAULT_FILE_PATH,get_commercial_license()),
    refresh_license(),
    timer:sleep(100),
    Result = chef_license_worker:get_license(),
    ?assertEqual({valid,undefined, undefined}, Result),
    os:cmd("rm -rf " ++ ?DEFAULT_FILE_PATH),
    
    file:write_file(?DEFAULT_FILE_PATH,get_commercial_license_expired()),
    refresh_license(),
    timer:sleep(100),
    Result1 = chef_license_worker:get_license(),
    ?assertEqual({expired,undefined, undefined}, Result1),
    os:cmd("rm -rf " ++ ?DEFAULT_FILE_PATH),
    
    file:write_file(?DEFAULT_FILE_PATH,get_commercial_grace_license()),
    refresh_license(),
    timer:sleep(100),
    Result2 = chef_license_worker:get_license(),
    ?assertEqual({valid,true,undefined}, Result2),
    os:cmd("rm -rf " ++ ?DEFAULT_FILE_PATH),

    file:write_file(?DEFAULT_FILE_PATH,get_trail_license()),
    refresh_license(),
    timer:sleep(100),
    Result3 = chef_license_worker:get_license(),
    ?assertEqual({valid, undefined, undefined}, Result3),
    os:cmd("rm -rf " ++ ?DEFAULT_FILE_PATH),

    file:write_file(?DEFAULT_FILE_PATH,get_trail_license_expired()),
    refresh_license(),
    timer:sleep(100),
    Result4 = chef_license_worker:get_license(),
    ?assertEqual({expired, undefined, undefined}, Result4),
    os:cmd("rm -rf " ++ ?DEFAULT_FILE_PATH).

refresh_license()->
    erlang:send(chef_license_worker, check_license).