-module(migrator_statement_cache).
-behaviour(gen_server).


get(Conn, TXTerm) ->
  gen_server:call(?MODULE, {get, Conn, TXTerm}).



%% Okay this is a bit weird - things got rushed (change of teams started now)
%% but this just
%%
