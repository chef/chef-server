%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%%
%% @author Marc Paradise <marc@opscode.com>
%% @copyright 2013, Opscode Inc
%%

-module(mover_user_hash_converter).

%% Comment this out for VIM syntax check/compiles to work.
-compile([{parse_transform, lager_transform}]).


-export([remaining_user_ids/0,
         convert_user/1,
         start_bcrypt_pool/0]).
%         start_bcrypt_worker_link/0]).

-record(tiny_user, {
        'username',         %% capture for debugging purposes.
        'serialized_object' %% that thing we're going to tear apart and reassemble
       }).

%% Retrieve a list of user ids who have not had their password hashes
%% migrated to the new schema and converted
remaining_user_ids() ->
    {ok, Data} = all_unconverted_users(),
    Data.

%% Stop any existing bcrypt worker pool and brin gup a new one.
%% This must be invoked prior to any attempt to convert_user/1.
start_bcrypt_pool() ->
    pooler:rm_pool(bcrypt),
    pooler:new_pool([{name, bcrypt},
                     {init_count, envy:get(mover, bcrypt_worker_count, integer)},
                     {max_count, envy:get(mover, bcrypt_worker_count, integer)},
                     {start_mfa, {gen_server, start_link,
                                  [bcrypt_nif_worker, [], []]}}]).

convert_user(Id) ->
    {ok, #tiny_user{username = Name, serialized_object = Object}} = user_data(Id),
    Json1 = chef_json:decode(Object),
    {SHA1Hash, Json2} = get_and_delete(<<"hashed_password">>, Json1),
    {Salt, Json3} = get_and_delete(<<"salt">>, Json2),
    {Type, NewSalt, NewHash} = convert_password_hash(SHA1Hash, Salt),
    Encoded = chef_json:encode(Json3),
    update_user_record(Name, Id, Type, NewHash, NewSalt, Encoded).

%%
%% Internal
%%
convert_password_hash(undefined, _Salt) ->
    % TODO type of None?
    {'SHA1', "", ""};
convert_password_hash(SHA1Hash, Salt) ->
    case pooler:take_member(bcrypt) of
        error_no_members ->
            {error, no_bcrypt_workers};
        Worker when is_pid(Worker) ->
            {ok, BcryptSalt} = gen_server:call(Worker, {gen_salt, 12}, infinity),
            % NOte that the original password went in as: OrigSalt ++ "--" ++ PlainPassword ++ "--",
            {ok, BcryptHash} = gen_server:call(Worker, {hashpw, SHA1Hash, BcryptSalt}, infinity),
            pooler:return_member(bcrypt, Worker),
            {'SHA1-bcrypt', Salt, BcryptHash}
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

update_user_record(_Name, Id, Type, Hash, Salt, Encoded) ->
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

user_update_sql() ->
    <<"UPDATE users
          SET hash_type = $2, hashed_password = $3, salt = $4, serialized_object = $5
        WHERE id = $1">>.


%%
%% SQL Statements
%%
all_unconverted_users_sql() ->
    <<"SELECT id FROM users WHERE hashed_password is null ORDER BY created_at">>.

