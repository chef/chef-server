-module(oc_erchef_dist).

-export([register_node/2]).
-include_lib("kernel/include/inet.hrl").
% Invoked via RPC when a new node wishes to become a front-end
% and needs appropriate config data to be provided to it.
% TODO we'll want to encrypt this data with a key generated
% during our own provisioning. But hey, PoC and all that.
%
register_node(Node, Role) ->
    spawn(fun() -> send_files(Node, Role) end),
    ok.

send_files(Node, Role) ->
    % So this temporary thing will block us from
    {ok, ListenSocket} = gen_tcp:listen(0, [{packet, 4}, {active,false}]),
    {ok, Port} = inet:port(ListenSocket),
    % Tihs was quick and dirty - some otheroptions include spawning an http server
    % that only accepts requests from this host, or adding a WM endpoointthat
    % does basic authentication by querying this gen-server for a temp auth token we provide,
    % etc, etc...
    {guest_node, Node} ! {proceed_registration, Role, Port},

    send_file_raw(Node, "/etc/opscode/chef-server.rb", ListenSocket, chef_server_rb(Node, Role)),
    ToSend = [ "/etc/opscode/webui_priv.pem", "/etc/opscode/webui_pub.pem",
               "/etc/opscode/pivotal.pem", "/etc/opscode/private-chef-secrets.json" ],
    [ send_file(Node, File, ListenSocket) || File <- ToSend ],
    gen_tcp:close(ListenSocket),
    {guest_node, Node}  ! registration_complete,
    ok.

% Cheating, sorry - once our own topology is actually an FE, we can
% just grab ours and replace the api_fqdn.  Instead we'll just give a suitable
    % file for the new node to treat us as a FA.
% file for t pretend like we're
% a backend,
chef_server_rb(NodeName, frontend) ->
    [_, FQDN] = re:split(atom_to_list(NodeName), "@", [{return, list}]),
    {ok,  #hostent{ h_addr_list = Ip}} = inet:gethostbyname(FQDN),
    Addr = inet:ntoa(hd(Ip)),
    D =
    [ <<"api_fqdn \"">>, FQDN, <<"\"\n">>,
      <<"topology \"tier\"\n">>,
      <<"server 'api.chef-server.dev',\n">>,
      <<"     ip_address: '192.168.33.100',\n">>,
      <<"     role: 'backend'\n">>,

      <<"server '">>, FQDN, <<"',\n">>,
      <<"     ip_address: '">>, Addr, <<"',\n">>,
      <<"     role: 'frontend'\n">>,

      <<"backend_vip 'api.chef-server.dev',\n">>,
      <<"     ipaddress: '192.168.33.100',\n">>,
      <<"     device: 'eth1'\n">>],
    lager:error("D: ~p", [D]),
    D.


send_file(Node, Name, ListenSocket) ->
    {ok, Bin} = file:read_file(Name) ,
    send_file_raw(Node, Name, ListenSocket, Bin).

send_file_raw(Node, FileName, ListenSocket, Data) ->
    {guest_node, Node} ! { config_data, FileName },
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    Result = gen_tcp:send(Socket, Data),
    lager:error("Send Result: ~p", [Result]),
    gen_tcp:close(Socket),
    ok.
