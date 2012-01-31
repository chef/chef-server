%% @author Tim Dysinger <timd@opscode.com>
%% @copyright 2012 Opscode, Inc. All Rights Reserved

-module(bookshelf_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_one, 10, 10}, []}}.
