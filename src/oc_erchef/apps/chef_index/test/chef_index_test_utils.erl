-module(chef_index_test_utils).

read_file(File) ->
    file:read_file(filename(File)).

filename(File) ->
    filename:join([".", "apps", "chef_index", "test", File]).

start_stats_hero() ->
    application:set_env(stats_hero, estatsd_host, "localhost"),
    application:set_env(stats_hero, estatsd_port, dumb_random_port()),
    application:set_env(stats_hero, udp_socket_pool_size, 5),
    case application:start(stats_hero) of
        ok ->
            ok;
        {error, {already_started, stats_hero}} ->
            ok
    end.

dumb_random_port() ->
    {ok, Socket} = gen_udp:open(0),
    {ok, Port} = inet:port(Socket),
    gen_udp:close(Socket),
    Port.

set_provider(solr) ->
    application:set_env(chef_index, search_provider, solr);
set_provider(elasticsearch) ->
    application:set_env(chef_index, search_provider, elasticsearch).
