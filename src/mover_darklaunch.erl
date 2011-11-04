%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin A. Smith <kevin@opscode.com>
%% @copyright 2011 Opscode, Inc.
-module(mover_darklaunch).

-export([update_darklaunch/3]).

update_darklaunch(Feature, Org, Value) ->
    {ok, Urls} = application:get_env(mover, darklaunch_urls),
    Res = scatter_to_all_darklaunch(Urls, Feature, Org, Value),
    case lists:all(fun(X) -> X =:= ok end, Res) of
        true -> ok;
        false -> error
    end.

scatter_to_all_darklaunch(Urls, Feature, Org, Value) ->
    Owner = self(),
    Pids = [spawn_link(make_darklaunch_update_worker(Owner, Url, Feature, Org, Value)) || Url <- Urls],
    gather_from_all_darklaunch(length(Pids), []).

gather_from_all_darklaunch(0, Results) ->
    Results;
gather_from_all_darklaunch(Count, Results) ->
    receive
        {darklaunch_result, Result} ->
            gather_from_all_darklaunch(Count - 1, [Result|Results])
    end.

make_darklaunch_update_worker(Owner, Url, Feature, Org, Value) ->
    fun() ->
            Result = post_to_darklaunch(Url, Feature, Org, Value),
            Owner ! {darklaunch_result, Result} end.



post_to_darklaunch(Url, Feature, Org, Value) when Value =:= true;
                                                  Value =:= false ->
    Url1 = binary_to_list(iolist_to_binary([Url, "/", Feature, "/", Org])),
    Body = iolist_to_binary(["{\"enabled\":", atom_to_list(Value), "}"]),
    Headers = [{"Content-Type", "application/json"},
               {"Accept", "application/json"}],
    IbrowseOpts = [{ssl_options, []}, {response_format, binary}],
    case ibrowse:send_req(Url1, Headers, post, Body, IbrowseOpts) of
        {ok, [$2, $0|_], _H, _Body} -> ok;
        Error ->
            log(err, "post_to_darklaunch failed ~s, enable:~s, reason:~256P",
                [Url1, Value, Error, 100]),
            error_logger:error_msg("post_to_darklaunch failed ~p", [Error]),
            {error, Error}
    end.

%% 11/3/2011 KAS: Filthy copy pasta but gotta get this working
%% for now :(
log(Level, Fmt, Args) when is_list(Args) ->
    Id = pid_to_list(self()),
    fast_log:Level(mover_manager_log, Id, Fmt, Args).
