-module(chefreq).

-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").

main([Path]) ->
    application:start(crypto),
    ssl:start(),
    ibrowse:start(),
    PFile = "/Users/seth/oc/environments/orgs/userprimary/.chef/seth.pem",
    {ok, PBin} = file:read_file(PFile),
    Private = chef_authn:extract_private_key(PBin),
    Client = chef_rest_client:make_chef_rest_client("https://api.chef.io", "seth", Private),
    {Url, Headers0} = chef_rest_client:generate_signed_headers(Client, Path, <<"GET">>),
    Headers = [{"Accept", "application/json"}, {"X-CHEF-VERSION", "0.10.0"} | Headers0],
    ibrowse:send_req(Url, Headers, get, [], [{ssl_options, []}]).

bad_time([Path]) ->
    application:start(crypto),
    ssl:start(),
    ibrowse:start(),
    PFile = "/Users/seth/oc/environments/orgs/userprimary/.chef/seth.pem",
    {ok, PBin} = file:read_file(PFile),
    Private = chef_authn:extract_private_key(PBin),
    Client = chef_rest_client:make_chef_rest_client("https://api.chef.io", "seth", Private),
    {Url, Headers0} = chef_rest_client:generate_signed_headers(Client, Path, <<"GET">>),
    Headers1 = lists:keyreplace("X-Ops-Timestamp", 1, Headers0,
                                {"X-Ops-Timestamp", "2011-06-21T19:06:35Z"}),
    Headers = [{"Accept", "application/json"}, {"X-CHEF-VERSION", "0.10.0"} | Headers1],
    ibrowse:send_req(Url, Headers, get, [], [{ssl_options, []}]).

missing([Path]) ->
    application:start(crypto),
    ssl:start(),
    ibrowse:start(),
    PFile = "/Users/seth/oc/environments/orgs/userprimary/.chef/seth.pem",
    {ok, PBin} = file:read_file(PFile),
    Private = chef_authn:extract_private_key(PBin),
    Client = chef_rest_client:make_chef_rest_client("https://api.chef.io", "seth", Private),
    {Url, Headers0} = chef_rest_client:generate_signed_headers(Client, Path, <<"GET">>),
    Headers1 = lists:keydelete("X-Ops-Timestamp", 1, Headers0),
    Headers2 = lists:keydelete("X-Ops-Content-Hash", 1, Headers1),
    Headers = [{"Accept", "application/json"}, {"X-CHEF-VERSION", "0.10.0"} | Headers2],
    ibrowse:send_req(Url, Headers, get, [], [{ssl_options, []}]).
    
bad_query() ->
    application:start(crypto),
    ssl:start(),
    ibrowse:start(),
    PFile = "/Users/seth/oc/environments/orgs/userprimary/.chef/seth.pem",
    {ok, PBin} = file:read_file(PFile),
    Private = chef_authn:extract_private_key(PBin),
    Client = chef_rest_client:make_chef_rest_client("https://api.chef.io", "seth", Private),
    {Url, Headers0} = chef_rest_client:generate_signed_headers(Client, "/organizations/userprimary/search/role?q=a[b", <<"GET">>),
    Headers = [{"Accept", "application/json"}, {"X-CHEF-VERSION", "0.10.0"} | Headers0],
    ibrowse:send_req(Url, Headers, get, [], [{ssl_options, []}]).

bad_start() ->
    application:start(crypto),
    ssl:start(),
    ibrowse:start(),
    PFile = "/Users/seth/oc/environments/orgs/userprimary/.chef/seth.pem",
    {ok, PBin} = file:read_file(PFile),
    Private = chef_authn:extract_private_key(PBin),
    Client = chef_rest_client:make_chef_rest_client("https://api.chef.io", "seth", Private),
    {Url, Headers0} = chef_rest_client:generate_signed_headers(Client, "/organizations/userprimary/search/role?q=ab&start=abc", <<"GET">>),
    Headers = [{"Accept", "application/json"}, {"X-CHEF-VERSION", "0.10.0"} | Headers0],
    ibrowse:send_req(Url, Headers, get, [], [{ssl_options, []}]).


user_not_in_org() ->
    application:start(crypto),
    ssl:start(),
    ibrowse:start(),
    PFile = "/Users/seth/oc/environments/orgs/userprimary/.chef/seth.pem",
    {ok, PBin} = file:read_file(PFile),
    Private = chef_authn:extract_private_key(PBin),
    Client = chef_rest_client:make_chef_rest_client("https://api.chef.io", "seth", Private),
    {Url, Headers0} = chef_rest_client:generate_signed_headers(Client, "/organizations/knifetest-org/search/role?q=ab&start=abc", <<"GET">>),
    Headers = [{"Accept", "application/json"}, {"X-CHEF-VERSION", "0.10.0"} | Headers0],
    ibrowse:send_req(Url, Headers, get, [], [{ssl_options, []}]).

no_user() ->
    application:start(crypto),
    ssl:start(),
    ibrowse:start(),
    PFile = "/Users/seth/oc/environments/orgs/userprimary/.chef/seth.pem",
    {ok, PBin} = file:read_file(PFile),
    Private = chef_authn:extract_private_key(PBin),
    Client = chef_rest_client:make_chef_rest_client("https://api.chef.io", "seth-xxx-yyy", Private),
    {Url, Headers0} = chef_rest_client:generate_signed_headers(Client, "/organizations/userprimary/search/role?q=ab&start=abc", <<"GET">>),
    Headers = [{"Accept", "application/json"}, {"X-CHEF-VERSION", "0.10.0"} | Headers0],
    ibrowse:send_req(Url, Headers, get, [], [{ssl_options, []}]).
    
