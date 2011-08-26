-module(chef_req).

-export([request/3, request/4, make_config/3, start_apps/0]).

-include("chef_req.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("chef_common/include/ej.hrl").
-include_lib("chef_common/include/chef_rest_client.hrl").

-define(gv(K, L), proplists:get_value(K, L)).

-export([main/1]).

main([]) ->
    Msg = "chef_req PATH\n\n"
        "Make Chef API requests\n"
        "Uses ./chef_req.config\n"
        "PATH example: /organizations/your-org/roles\n",
    io:format(Msg);
main([Path]) ->
    ok = start_apps(),
    %% FIXME: for now, config file location is  hard coded
    {ok, Config} = file:consult("chef_req.config"),

    KeyPath = ?gv(private_key, Config),
    ApiRoot = ?gv(api_root, Config),
    Name = ?gv(client_name, Config),
    ReqConfig = make_config(ApiRoot, Name, KeyPath),

    {ok, Code, Head, Body} = request(get, Path, ReqConfig),
    io:format(standard_error, "~s ~s~n", [Code, Path]),
    io:format(standard_error, "~s~n", ["----------------"]),
    [ io:format(standard_error, "~s:~s~n", [K, V])
      || {K, V} <- Head ],
    io:format(standard_error, "~s~n", ["----------------"]),
    io:format("~s~n", [Body]).

request(get, Path, ReqConfig) ->
    request(get, Path, [], ReqConfig).

request(Method, Path, Body,
        #req_config{api_root = ApiRoot, name = Name, private_key = Private}) ->
    {Url, Headers} = make_headers(method_to_bin(Method), ApiRoot, Path,
                                  Name, Private, Body),
    ibrowse:send_req(Url, Headers, Method, Body, [{ssl_options, []}]).

make_config(ApiRoot, Name, KeyPath) ->
    {ok, PBin} = file:read_file(KeyPath),
    Private = chef_authn:extract_private_key(PBin),
    #req_config{api_root = ApiRoot, name = Name, private_key = Private}.

load_config(Path) ->
    {ok, Config} = file:consult(Path),
    PrivatePath  = ?gv(private_key, Config),
    ApiRoot = ?gv(api_root, Config),
    Name = ?gv(client_name, Config),
    make_config(ApiRoot, Name, PrivatePath).

start_apps() ->
    [ ensure_started(M) || M <- [crypto, public_key, ssl] ],
    case ibrowse:start() of
        {ok, _} -> ok;
        {error,{already_started, _}} -> ok
    end,
    ok.

ensure_started(M) ->
    case application:start(M) of
        ok ->
            ok;
        {error,{already_started,_}} ->
            ok;
        Error ->
            Error
    end.

method_to_bin(get) ->
    <<"GET">>;
method_to_bin(put) ->
    <<"PUT">>;
method_to_bin(post) ->
    <<"POST">>;
method_to_bin(delete) ->
    <<"DELETE">>;
method_to_bin(head) ->
    <<"HEAD">>.

load_private_key(Config) ->
    PFile = ?gv(private_key, Config),
    {ok, PBin} = file:read_file(PFile),
    chef_authn:extract_private_key(PBin).

make_headers(Method, ApiRoot, Path, Name, Private, Body) ->
    Client = chef_rest_client:make_chef_rest_client(ApiRoot, Name, Private),
    {Url, Headers0} = chef_rest_client:generate_signed_headers(Client, Path,
                                                               Method),
    Headers1 = header_for_body(Body, Headers0),
    {Url, [{"Accept", "application/json"},
           {"X-CHEF-VERSION", ?CHEF_VERSION} | Headers1]}.

header_for_body([], Headers) ->
    Headers;
header_for_body(_, Headers) ->
    [{"content-type", "application/json"}|Headers].
