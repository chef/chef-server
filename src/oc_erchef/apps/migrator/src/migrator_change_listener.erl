%% ex: ts=4 sw=4 et
%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-

-module(migrator_change_listener).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([%% API functions
         start_link/0,
         startup/0,

         %% gen_server behaviour
         init/1,
         handle_call/3,
         handle_cast/2,
         terminate/2,
         handle_info/2,
         code_change/3]).

%{ok, C2} = epgsql:connect("127.0.0.1", "opscode-pgsql", Password, [{database, "opscode_chef"}]).
%epgsql:squery(C2, "select * from pg_logical_slot_get_changes('regress ion_slot', '0/3427C8A0', NULL)").
%location: lsn
%xid: transaction id
%data: text
%
% {ok, Password} = chef_secrets:get(<<"postgresql">>, <<"db_superuser_password">>),
%{ok, C2} = epgsql:connect("127.0.0.1", "opscode-pgsql", Password, [{database, "opscode_chef"}]).
% D2 = epgsql:squery(C2, "select * from pg_logical_slot_get_changes('regression_slot',NULL, NULL, ['include-xids','force-binary', 'skip-empty-xacts'])").
% {ok, Def, Data} = D1.% API
% F | Rest = Data -> one item per change
% Each record is a tuple:
%   {LSN, XID, DATA}
%
% {LSN, XID, Raw} = First.
%
%
% AllRaw = [Data || {_, _, Data} <- Remaining].
%
% Raw ex:
%<<"table public.orgs: INSERT: id[character]:'588a6e1a3944129a89e9d5e2f1f1cec5' authz_id[character]:'3b214ce9d7f790dccebbd2af33711977' name[text]:'pedant_testorg_api_23439' full_name[text]:'pedant_testorg_api_23439' assigned_at[timestamp without time zone]:'2017-06-05 17:16:06' last_updated_by[character]:'c3fbae8bae3f5677e319a8282bc180f2' created_at[timestamp without time zone]:'2017-06-05 17:16:06' updated_at[timestamp without time zone]:'2017-06-05 17:16:06'">>
%Multiline example that includes json:
%
%<<"table public.users: INSERT: id[character]:'0000000000009cd8a4dcbba29e15b0c0' authz_id[character]:'f5477ed38d0fae9f9547dea9f40a24f7' username[text]:'pedant_testorg_api_23439_owner' email[text]:'pedant_testorg_api_23439_owner@chef.io' pubkey_version[integer]:0 public_key[text]:'-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA17bbwaecnsRq44XYvknR\nUGS2PSoz4zmJMYC7vyrijHxTyY7m6q8vzeUh7mL38o3FqkcqoffQ1Vfu0THtlqsv\nbyW/nqbV0OGSmX22YPa4Fjg7aLbQ2++chxG9NBzQGrQNn54+DNipMWNSZyAU6bbd\nXgXnmBTxiaqIpLZSJbZB0NKZYTpRd1GYrJAYmquYs0s+WCdTRH6Ebp1NgIWjpVis\n8I7TE42dcndCp5DcN1xP4MQV6YiqM0Ss7Tmv+XcUsgOkXUTJocsi6OI8d3c+QFWh\nUBZEXxiBtKVN5YS1+IeaQBsuNBIZi2qTEcgIQQxvFnIGjGYU5qrP3jzq6Q1EEYWe\nhwIDAQAB\n-----END PUBLIC KEY-----\n\n' serialized_object[text]:'{\"display_name\":\"pedant_testorg_api_23439_owner\",\"first_name\":\"pedant_testorg_api_23439_owner\",\"last_name\":\"pedant_testorg_api_23439_owner\"}' last_updated_by[character]:'c3fbae8bae3f5677e319a8282bc180f2' created_at[timestamp without time zone]:'2017-06-05 17:16:07' updated_at[timestamp without time zone]:'2017-06-05 17:16:07' external_authentication_uid[text]:null recovery_authentication_enabled[boolean]:false admin[boolean]:false hashed_password[text]:'$2a$12$Mzx1MLByrorRdHFDebOohebyAvtapLvg/h7ZOStu/u8/.jrsEhlaK' salt[text]:'$2a$12$Mzx1MLByrorRdHFDebOohe' hash_type[password_hash_type]:'bcrypt'">>
%The data we care about will be:
%BEGIN $txid -> new tx, maybe track it but move on.
%table $schema.name
% ^ $schema in our use will always be 'public' except for sqitch tracking, which will be
%   'sqitch'
%COMMIT $txid -> end of that tx
%*******
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

% Temp separating this from init for easier debugging.
startup() ->
    gen_server:call(?SERVER, startup).

%% gen_server functions
init(_Args) ->

    %% Doing a port for now. May be sufficient, but might also wwant to considerj
    %% a direct pgsql connection to the DB as an appropriate user. This will simplify
    %% handling of begin/end TX.
    {ok, Password} = chef_secrets:get(<<"postgresql">>, <<"db_superuser_password">>),
    CommandArgs = ["--start", "-h", "127.0.0.1",
                   "-U", "opscode-pgsql",
                   "-S", "regression_slot", % slot name - you *did* remember to run
                   % SELECT * FROM pg_create_logical_replication_slot('migration_slot', 'test_decoding');
                   %                        % didn't you? Come back when you're ready.
                   "-f", "-", % out to stdout
                   "-d", "opscode_chef",
                   % The options below are taken from here:
                   % https://doxygen.postgresql.org/test__decoding_8c.html#ae9ad7b4e35a8ca2df15233e16557cbcf
                   "-o", "skip-empty-xacts",
                   "-o", "force-binary", % experimenting to see the diff it makes
                   "--no-password"],

    PortSettings = [
                    {args,CommandArgs},
                    {env, [{"PGPASSWORD", binary_to_list(Password)}]},
                    use_stdio,
                    binary,
                    {line, 1024}
                   ],
    % Observation - when too much data backlogs in stdout, pg_recvlogical fails to write?!
    Port = open_port({spawn_executable,
                      "/opt/opscode/embedded/bin/pg_recvlogical"}, PortSettings),
    {ok, #{port => Port, txid => undefined}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

% Lazily not handling neol - looks like nothing approaches the arbitrary 1k I gave it above.
% In the real world, this wouldn't do...
handle_info({_Port, {data, {eol, Message}}}, State) ->
    {noreply, State};
handle_info({_Port, {data, Message}}, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal
%%
%%

% Formatting notes:
%
% This is what accounts for the basic format of attributes/columns:
% https://doxygen.postgresql.org/test__decoding_8c.html#a3986a57a0308de0150ebd45f7734d464
%
% The rough shape appears to be:
%
% BEGIN $txid
% table $schemaname.$tablename: $fieldname1[$datatype1]:'$value1' $fieldname2[$datatype2]:'$value2' $fieldnamen[$datatypen]:'$valuen'
% COMMIT $txid
%
% Observations:
% * literal 'table' can presumably be other things when appropriate
% * VALUE can span multiple lines when a literal newline is present in the data.
           % it can also presumable include : and ' - so how do we determine
           % when we're at the end of hte field...
% * subsequent fields can be appended to the end of the same line of a multi-line value
%
% So far it does not seem that TX data is intermixed, and reading the decoding code
% seems to agree.
%
% Looks like we can do this: use the pg_recv_logical function
% pg_logical_slot_get_changes which returns LSN, XID, and DATA
% one record per change within the TX
% Within the data we are guaratneed the following:
%   1st: data type.
%
% Data fields are not fixed width so binary matching won't help us much here.
%

%slurp_data(<<"BEGIN ", _TXID/binary>>) ->
    %% Ideally we'd track this txid in state and reconcile it
    %lager:info("Begin TX ~p", [_TXID]),
    %ok;
%slurp_data(<<"COMMIT ", _TXID/binary>>) ->
    %% This is where we'd clear it from state...
    %lager:info("Commit TX ~p", [_TXID]),
    %ok;
%slurp_data(<<"table: ", Rest/binary>>) ->
    %% Here is where things get fun. We could have next a single statement on a single line -
    %% or it could be the beginning of a statement that spans mutiple lines
    %% table $schemaname.$tablename: $fieldname1[$datatype1]:'$value1' $fieldname2[$datatype2]:'$value2' $fieldnamen[$datatypen]:'$valuen'
    %slurp_table_update(Rest);
%slurp_data(All) ->
    %% here we'd append data to the last value received - it's a continuation.
    %% At least assuming that "table" is the only leader/object type we'll see...
    %lager:info(">> ~p", [All]).

%slurp_table_update(<<Action/binary, ":">>) ->
    %ok.


%%
%%
