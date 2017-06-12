%% This applies SQL updates to the target system directly. It maintains its own connection.
%% We may look at having multiple instances of these (one per DB or more) down the road.
-module(migrator_target_db).
-behaviour(gen_server).


bin_cache_get(TXTerm) ->



% When you invoke apply/1:
%
% This module maintains a two-level cache:
% 1. statements converted to binary strings with placeholders, suitable for preparing
% 2. per-postgres-connection prepared statement references
%
%
%  - check the per-connection cache for the statement. Return that reference if it exists, otherwise:
%  - check the binary cache for a pre-assembled binary to represent this combination
%    of entity, operation, and fields.
%       - if nothing is found, parse the statement into this form and cache it in the binary cache
%    - execute postgres prepare operations for the statement and cache the reference to it.
%  Return the prepared statement reference to the caller.
%
%
%  The binary cache is keyed to phash2(InputTXTerm). This cache is kept in an ets table
%  so that it will survive restarts of the cache process.
%
%  The prepared statement reference cache is keyed to {ConnId, phash2(InputTxTerm)}
%
%  Any connection that uses this service is monitored; if it terminates,
%  the associated prepared statement cache will be destroyed
