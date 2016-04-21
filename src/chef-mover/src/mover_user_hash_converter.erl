%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%%
%% @author Marc Paradise <marc@chef.io>
%% @copyright 2013, Opscode Inc
%%

-module(mover_user_hash_converter).

-export([remaining_user_ids/0,
         convert_user/1,
         start_bcrypt_pool/0,
         bcrypt_workers_count_config/0,
         all_unconverted_users_count/0,
         all_users_count/0]).

-record(tiny_user, {
        'username',         %% capture for debugging purposes.
        'serialized_object' %% that thing we're going to tear apart and reassemble
       }).

%% Retrieve a list of user ids who have not had their password hashes
%% migrated to the new schema and converted
remaining_user_ids() ->
    {ok, Data} = all_unconverted_users(),
    Data.

%% Stop any existing bcrypt worker pool and bring up a new one.
%% This must be invoked prior to any attempt to convert_user/1.
start_bcrypt_pool() ->
    pooler:rm_pool(bcrypt),
    pooler:new_pool([{name, bcrypt},
                     {init_count, bcrypt_workers_count_config()},
                     {max_count, bcrypt_workers_count_config()},
                     % We will start the worker directly via gen_server, since
                     % the start_link included in bcrypt_nif_worker will create only
                     % a single named worker
                     {start_mfa, {gen_server, start_link,
                                  [bcrypt_nif_worker, [], []]}}]).

convert_user(Id) ->
    {ok, #tiny_user{serialized_object = Object}} = user_data(Id),
    Json1 = chef_json:decode(Object),
    {SHA1Hash, Json2} = get_and_delete(<<"hashed_password">>, Json1),
    {Salt, Json3} = get_and_delete(<<"salt">>, Json2),
    {Type, NewSalt, NewHash} = convert_password_hash(SHA1Hash, Salt),
    Encoded = chef_json:encode(Json3),
    update_user_record(Id, Type, NewHash, NewSalt, Encoded).

bcrypt_workers_count_config() ->
    envy:get(mover, bcrypt_worker_count, integer).

%%
%% Internal
%%
convert_password_hash(<<>>, Salt) ->
    convert_password_hash(undefined, Salt);
convert_password_hash(undefined, _Salt) ->
    % User password is unknown, so they cna't log in anyway.
    % Let's generate a new password so that we don't leave their account
    % unprotected.
    NewPass = base64:encode_to_string(crypto:rand_bytes(18)),
    {ok, BCryptHash} = hash_password(NewPass),
    {bcrypt, "", BCryptHash};
convert_password_hash(SHA1Hash, Salt) ->
    {ok, BcryptHash} = hash_password(SHA1Hash),
    {'SHA1-bcrypt', Salt, BcryptHash}.

hash_password(Password) ->
    case pooler:take_member(bcrypt) of
        error_no_members ->
            {error, no_bcrypt_workers};
        Worker when is_pid(Worker) ->
            Rounds = envy:get(mover, bcrypt_encryption_rounds, integer),
            {ok, BcryptSalt} = gen_server:call(Worker, {gen_salt, Rounds}, infinity),
            {ok, BcryptHash} = gen_server:call(Worker, {hashpw, Password, BcryptSalt}, infinity),
            pooler:return_member(bcrypt, Worker),
            {ok, BcryptHash}
    end.

user_data(Id) ->
    case sqerl:execute(<<"SELECT username, serialized_object FROM users WHERE id = $1">>, [Id]) of
        {error, Error} ->
            lager:error("Failed to fetch user detail: ~p", [Error]),
            {error, Error};
        {ok, Rows } when is_list(Rows) ->
            XF = sqerl_transformers:rows_as_records(tiny_user, record_info(fields, tiny_user)),
            {ok, [Data]} = XF(Rows),
            {ok, Data}
    end.

get_and_delete(Key, Json) ->
    Value = ej:get({Key}, Json),
    Final = ej:delete({Key}, Json),
    {Value, Final}.

update_user_record(Id, Type, Hash, Salt, Encoded) ->
    case sqerl:execute(user_update_sql(), [Id, Type, Hash, Salt, Encoded]) of
        {ok, _Num} ->
            ok;
        {error, Error} ->
            lager:error("User updated failed: ~p", [Error]),
            {error, Error}
    end.

all_unconverted_users() ->
    case sqerl:execute(all_unconverted_users_sql(), []) of
        {error, Error} ->
            lager:error("Failed to fetch user list: ~p", [Error]),
            {error, Error};
        {ok, []} ->
            {ok, []};
        {ok, Rows } when is_list(Rows) ->
            XF = sqerl_transformers:rows_as_scalars(id),
            XF(Rows)
    end.

all_unconverted_users_count() ->
    get_count(all_unconverted_users_count_sql()).

all_users_count() ->
    get_count(all_users_count_sql()).

get_count(Statement) ->
    case sqerl:select(Statement, [], first_as_scalar, [count]) of
        {error, {Code, Message}} ->
            {error, {sqerl_pgsql_errors:translate_code(Code), Message}};
        {ok, Count} ->
            Count;
        Other ->
            Other
    end.

%%
%% SQL Statements
%%
user_update_sql() ->
    <<"UPDATE users
          SET hash_type = $2, hashed_password = $3, salt = $4, serialized_object = $5
        WHERE id = $1">>.

all_unconverted_users_sql() ->
    <<"SELECT id FROM users WHERE hashed_password is null ORDER BY created_at">>.

all_unconverted_users_count_sql() ->
    <<"SELECT count(*) FROM users WHERE hashed_password is null">>.

all_users_count_sql() ->
    <<"SELECT count(*) FROM users">>.


