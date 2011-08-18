-module(chef_req).

-export([main/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("chef_common/include/ej.hrl").
-include_lib("chef_common/include/chef_rest_client.hrl").

-define(gv(K, L), proplists:get_value(K, L)).

%% version used in X-CHEF-VERSION header sent to server
-define(CHEF_VERSION, "0.10.0").

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

    Private = load_private_key(Config),
    ApiRoot = ?gv(api_root, Config),
    Name = ?gv(client_name, Config),

    {ok, _Code, Head, Body} = request(get, ApiRoot, Path, Name, Private),
    io:format(standard_error, "~s~n", [Path]),
    [ io:format(standard_error, "~s:~s~n", [K, V])
      || {K, V} <- Head ],
    io:format("~s~n", [Body]).

start_apps() ->
    application:start(crypto),
    ssl:start(),
    ibrowse:start(),
    ok.

request(get, ApiRoot, Path, Name, Private) ->
    request(get, ApiRoot, Path, Name, Private, []).

request(Method, ApiRoot, Path, Name, Private, Body) ->
    {Url, Headers} = make_headers(method_to_bin(Method), ApiRoot, Path,
                                  Name, Private, Body),
    ibrowse:send_req(Url, Headers, Method, Body, [{ssl_options, []}]).

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
