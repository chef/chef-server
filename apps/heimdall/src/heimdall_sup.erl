-module(heimdall_sup).

-behaviour(supervisor).

%% External exports
-export([
         start_link/0,
         upgrade/0
        ]).

%% supervisor callbacks
-export([
         init/1
        ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% NOTE: This is the same as found in chef_wm_sup
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      %%% We wish to continue even if a child isn't found.
                      _ = supervisor:terminate_child(?MODULE, Id),
                      _ = supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

init([]) ->

    {ok, Ip} = application:get_env(heimdall, ip),
    {ok, Port} = application:get_env(heimdall, port),
    {ok, Dispatch} = file:consult(filename:join([code:priv_dir(heimdall),
                                                 "dispatch.conf"])),

    WebConfig = [
                 {ip, Ip},
                 {port, Port},
                 {log_dir, "priv/log"},
                 {dispatch, add_superuser_id(Dispatch)}
                ],

    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, dynamic},

    Processes = [Web],
    {ok, {{one_for_one, 10, 10}, Processes}}.

%% Here we're adding the superuserID once when we set up the endpoints in webmachine
%% so we don't have to do this repeatedly for every request
superuser_to_config([], SuperuserId) ->
    [];
superuser_to_config([{Path, Module, Args}|Rest], SuperuserId) ->
    NewConfig = {superuser_id, SuperuserId},
    [{Path, Module, [NewConfig | Args]} | superuser_to_config(Rest, SuperuserId)].

add_superuser_id(Dispatch) ->
    {ok, SuperuserId} = application:get_env(heimdall, superuser_id),
    superuser_to_config(Dispatch, SuperuserId).
