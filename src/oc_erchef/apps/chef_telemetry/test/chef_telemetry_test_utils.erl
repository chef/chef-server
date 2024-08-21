-module(chef_telemetry_test_utils).

-export([
    start_server/1
    ]).

start_server(_Inputs) ->
    application:start(inets),
    {_Httpd_State, _Httpd_Pid} = 
        inets:start(httpd, [{port, 9001}, 
                            {server_name, "localhost"}, {document_root, "/tmp"}, 
                            {modules,[mod_esi]},{server_root, "/tmp"}, {erl_script_alias, {"/esi", [payload]}}]).
