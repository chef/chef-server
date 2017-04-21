%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%%
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@chef.io>
%% Copyright 2016 Chef Software, Inc.
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

-module(oc_chef_authz_scoped_name).

-include("chef_types.hrl").
-include("oc_chef_types.hrl").

-export([names_to_authz_id/3,
         initialize_context/2,
         initialize_context/1,
         full_name/1,

         %% Used by oc_chef_group
         find_client_authz_ids/2,
         find_user_authz_ids/2,
         find_group_authz_ids/2,
         convert_ids_to_names/3
        ]).

-export([parse_scoped_name/3]).

-ifdef(TEST).
-compile([export_all]).
-endif.

%%
%% Process names with scoping descriptor.
%%

%% This is derived from oc_chef_wm/src/oc_chef_wm_groups.erl; investigate what it will take to refactor this.
-define(NAME, "[a-z0-9\-_]").

-define(SCOPE_SEPARATOR, <<"::">>).
-define(SCOPED_NAME_REGEX, "^(?:(" ?NAME "+)|(?:(" ?NAME "*)\\:\\:(" ?NAME "+)))$").

-type db_callback() :: fun((any()) -> any()).

-record(context, {org_id :: binary(),
                  db_context :: any() | undefined,
                  db_callback_fun :: db_callback()
                 }).

-record(sname, {full :: binary(),
                base :: binary() | undefined,
                org  :: binary() | 'global_org' | undefined,
                org_id :: binary() | undefined,
                authz_id :: binary() | undefined
               }).

-type lookup_type() :: 'actor' | 'user' | 'client' | 'group'.

%%
%% Accessors for Scope name record
%%
-spec full_name(#sname{}) -> binary().
full_name(#sname{full = FullName}) ->
    FullName.

%%
%% Context is used to thread external state information through the system
%%
%% Dialyzer is grouchy unless undefined is included in types.
-spec initialize_context(binary()) -> #context{}.
initialize_context(OrgId) ->
    initialize_context(OrgId, make_sql_callback()).

-spec initialize_context(binary(), db_callback()) -> #context{}.
initialize_context(OrgId, CallBackFun) ->
    initialize_context(OrgId, undefined, CallBackFun).

-spec initialize_context(binary(), tuple() | undefined, db_callback()) -> #context{}.
initialize_context(OrgId, DbContext, CallBackFun) ->
    #context{org_id = OrgId,
             db_context = DbContext,
             db_callback_fun = CallBackFun}.

%%
%% Takes scoped names to authz ids
%%   Names: list of names as binary; can include scoping symbol
%%   Type: The type of object to look for
%%   OrgContext: The name of the org whose context is local or 'global'
%%
%% We perform a series of successive lowerings;
%% * parsing the name into base and org components
%%   (if the later is present, otherwise inserting the current in-context org)
%% * Mapping the org name to id
%% * Looking up the org id, name pair in the appropriate table(s)

%%
%% Output:
%% { [{Name, AuthzId}, [{Name, ErrorType}] }
%%
%%
-spec names_to_authz_id(lookup_type() ,[binary()], #context{}) -> { [binary()],[{atom(),binary()}] }.
names_to_authz_id(Type, Names, MapperContext) ->
    %% Lower to fully qualified orgname, name
    ScopedNames = parse_scoped_names(Names, is_scoped_type(Type), MapperContext),
    {ProperNames, Errors} = lists:foldl(fun filter_errors/2, {[], []}, ScopedNames),

    OrgCache = init_org_name_cache(),

    %% Map org names to org ids
    {NamesWithOrgIds, Errors2, _} =
        lists:foldl(fun(Name, Acc) -> lookup_org_id(Name, Acc) end,
                    {[], Errors, OrgCache}, ProperNames),

    %% group by org id for efficiency (can't do it sooner because we don't know all the org ids
    NamesGroupedByOrgIds = group_by_org_ids(NamesWithOrgIds),

    %% Take the grouped records, and look them up
    {AuthzIds, Errors3} = lists:foldl(fun(N, A) ->
                                               scoped_names_to_authz_id(Type, N, A)
                                       end,
                                       {[], Errors2}, NamesGroupedByOrgIds),

    {lists:flatten(AuthzIds), group_errors(Errors3)}.


%%
%% Reverse mapping of ids to names
%%
convert_ids_to_names(ActorAuthzIds, GroupAuthzIds, Context) ->
    {ClientNames, RemainingAuthzIds} = authz_id_to_names(client, ActorAuthzIds, Context),
    {UserNames, DefunctActorAuthzIds} = authz_id_to_names(user, RemainingAuthzIds, Context),
    {GroupNames, DefunctGroupAuthzIds} = authz_id_to_names(group, GroupAuthzIds, Context),
    oc_chef_authz_cleanup:add_authz_ids(DefunctActorAuthzIds, DefunctGroupAuthzIds),
    {ClientNames, UserNames, GroupNames}.

%% Helper functions
%%
%% No error handling; we probably should generate an error when we have missing
%%
-spec find_client_authz_ids([binary()],#context{org_id::binary(), db_callback_fun::db_callback()}) -> [binary()].
find_client_authz_ids(ClientNames, Context) ->
    {AuthzIds, _Missing} = names_to_authz_id(client, ClientNames, Context),
    AuthzIds.

-spec find_user_authz_ids([binary()],#context{org_id::binary(), db_callback_fun::db_callback()}) -> [binary()].
find_user_authz_ids(UserNames, Context) ->
    {AuthzIds, _Missing} = names_to_authz_id(user, UserNames, Context),
    AuthzIds.

-spec find_group_authz_ids([binary()],#context{org_id::binary(), db_callback_fun::db_callback()}) -> [binary()].
find_group_authz_ids(GroupNames, Context) ->
    {AuthzIds, _Missing} = names_to_authz_id(group, GroupNames, Context),
    AuthzIds.

%%
%% Lookup org name
%%
-spec org_id_to_name(binary()) -> not_found | binary().
org_id_to_name(OrgId) ->
    %% TODO maybe rework this; it bypasses a bunch of our statistics gathering code.
    case chef_sql:select_rows({find_organization_by_id, [OrgId]}) of
        [Org|_Others] ->  proplists:get_value(<<"name">>, Org);
        _ -> not_found
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%

%% Default sql callback
make_sql_callback() ->
    fun chef_sql:select_rows/1.

group_errors(Errors) ->
    group_by_key(lists:flatten(Errors)).

filter_errors(#sname{} = ScopedName, {Parsed, Errors}) ->
    { [ScopedName | Parsed], Errors };
filter_errors(Error, {Parsed, Errors}) ->
    {Parsed, [Error | Errors]}.

%% Lookup org_ids from orgnames
%%
%% already have id for orgname, skip
%% Errors: orgname_not_found
lookup_org_id(#sname{org_id = OrgId} = Name, {AccNames, Errors, Cache}) when OrgId =/= undefined ->
    { [Name | AccNames], Errors, Cache };
%% Need to lookup name
lookup_org_id(#sname{org = OrgName} = Name, {AccNames, Errors, Cache}) ->
    case lookup_org_id_cached(OrgName, Cache) of
        {not_found, Cache1} ->
            {AccNames, [{orgname_not_found, Name} | Errors], Cache1};
        {OrgId, Cache1} ->
            {[Name#sname{org_id = OrgId} | AccNames], Errors, Cache1}
    end.

%%
%% Group by org id
%%
group_by_org_ids(Names) ->
    NamesByOrgId = lists:foldl(fun group_by_org_ids/2, #{}, Names),
    maps:to_list(NamesByOrgId).

group_by_org_ids(#sname{org_id = OrgId} = Name, Acc) ->
    Old = maps:get(OrgId, Acc, []),
    maps:put(OrgId, [Name | Old], Acc).

%%
%% Actually lookup names
%%
%% Errors: not_found, ambiguous
scoped_names_to_authz_id(Type, {OrgId, Names}, {AuthzIdAcc, Errors}) ->
    BaseNames = [ Base || #sname{base = Base} <- Names],
    {AuthzIds, Missing, Ambiguous} = authz_records_by_name(Type, OrgId, BaseNames),
    MissingErrors = make_error_from_set(not_found, Names, Missing),
    AmbiguousErrors = make_error_from_set(ambiguous, Names, Ambiguous),
    {[AuthzIds | AuthzIdAcc],  [AmbiguousErrors | [MissingErrors | Errors]]}.

make_error_from_set(ErrorName, ScopedNames, UnscopedNameSet) ->
    [ {ErrorName, Name} || Name <- extract_full_names(ScopedNames, UnscopedNameSet) ].

extract_full_names(ScopedNames, UnscopedNameSet) ->
    [ Full || #sname{full = Full, base = Base} <- ScopedNames,
              ordsets:is_element(Base, UnscopedNameSet) ].

%%
%% Abstract away difference in lookups between actors, clients, users and groups.
%%
%% Returns AuthzIds, missing names and names that are judged ambiguous
authz_records_by_name(actor, OrgId, Names) ->
    {ok, Actors} = oc_chef_authz_db:find_org_actors_by_name(OrgId, Names),
    {Missing, Remaining} = lists:partition(fun is_missing_actor/1, Actors),
    {Ambiguous, Valid} = lists:partition(fun is_ambiguous_actor/1, Remaining),
    AuthzIds = ids_from_records(Valid),
    {AuthzIds,
     ordsets:from_list(names_from_records(Missing)),
     ordsets:from_list(names_from_records(Ambiguous))};
authz_records_by_name(Type, OrgId, Names) ->
    Records = oc_chef_authz_db:authz_records_by_name(Type, OrgId, Names),
    AuthzIds = ids_from_records(Records),
    FoundNames = ordsets:from_list(names_from_records(Records)),
    GivenNames = ordsets:from_list(Names),
    Remaining = ordsets:subtract(GivenNames, FoundNames),
    {AuthzIds, Remaining, ordsets:new()}.

%% Helper functions for oc_chef_authz_db:authz_records_by_name and oc_chef_authz_db:find_org_actors_by_name
-spec names_from_records([{binary(),_} | {binary(),_,_}]) -> [binary()].
names_from_records(Records) ->
    [ name_from_record(R) || R  <- Records].

-spec name_from_record({binary(),_} | {binary(),_,_}) -> binary().
name_from_record({Name, _,  _}) ->
    Name;
name_from_record({Name, _}) ->
    Name.

-spec ids_from_records([{_,binary()} | {_,'null' | binary(),'null' | binary()}]) -> [binary()].
ids_from_records(Records) ->
    [ id_from_record(R) || R <- Records ].

-spec id_from_record({_, binary()} | {_, binary()|null, binary()|null}) -> binary().
id_from_record({_, AuthzId}) ->
    AuthzId;
id_from_record({_, UserAuthzId, null}) ->
    UserAuthzId;
id_from_record({_, null, ClientAuthzId}) ->
    ClientAuthzId.

is_missing_actor({_, null, null}) ->
    true;
is_missing_actor({_, _, _}) ->
    false.

is_ambiguous_actor({_, UserAZ, ClientAZ}) when UserAZ =/= null andalso
                                               ClientAZ =/= null ->
    true;
is_ambiguous_actor({_, _, _}) ->
    false.

%%
%% Takes authz ids to scoped names
%%
%% Returns {NamesFound, UnmappedAuthzIds} We can have UnmappedAuthzIds if an entity was
%% deleted on the server but not in bifrost, or if we have a mix of clients and users
%%
%% Each type of object has different restrictions on its scope.
%%
-spec authz_id_to_names('client' | 'group' | 'user', [binary()],
                        #context{org_id::binary(), db_callback_fun::db_callback()}) ->
        {[binary()],[binary()]}.
authz_id_to_names(group, AuthzIds, #context{org_id = OrgId, db_callback_fun = CallbackFun}) ->
    {ScopedNames, DiffedList} = query_and_diff_authz_ids(find_scoped_group_name_in_authz_ids, AuthzIds, CallbackFun),
    {render_names_from_org_id(OrgId, ScopedNames), DiffedList};
authz_id_to_names(client, AuthzIds, #context{db_callback_fun = CallbackFun}) ->
    query_and_diff_authz_ids(find_client_name_in_authz_ids, AuthzIds, CallbackFun);
authz_id_to_names(user, AuthzIds, #context{db_callback_fun = CallbackFun}) ->
    query_and_diff_authz_ids(find_user_name_in_authz_ids, AuthzIds, CallbackFun).

query_and_diff_authz_ids(_QueryName, [], _) ->
    %% Sometimes the list of authz ids is empty; shortcut that and save a DB call.
    {[], []};
query_and_diff_authz_ids(QueryName, AuthzIds, CallbackFun) ->
    case CallbackFun({QueryName, [AuthzIds]}) of
        not_found ->
            {[], AuthzIds};
        Results when is_list(Results)->
            {ResultNames, FoundAuthzIds} = lists:foldl(fun extract_maybe_scoped_name/2,
                                                       {[],[]}, Results),
            DiffedList = sets:to_list(sets:subtract(sets:from_list(AuthzIds), sets:from_list(FoundAuthzIds))),
            {lists:sort(ResultNames), DiffedList};
        _Other ->
            {[], []}
    end.

%% Scoped names are triples with org_id, name, and authz_id
extract_maybe_scoped_name([{_NameKey, Name}, {<<"authz_id">>, AuthzId}],
                          {NamesIn, AuthzIdsIn}) ->
    {[Name | NamesIn], [AuthzId | AuthzIdsIn]};
extract_maybe_scoped_name([{<<"org_id">>, OrgId}, {_NameKey, Name}, {<<"authz_id">>, AuthzId}],
                          {NamesIn, AuthzIdsIn}) ->
    {[{OrgId, Name} | NamesIn], [AuthzId | AuthzIdsIn]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Support routines
%%
-spec is_scoped_type(lookup_type()) -> boolean().
is_scoped_type(group) ->
    true;
is_scoped_type(_) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tools for parsing/unparsing scoped names into {orgname, name} tuples
%% These use a org name as 'context' to resolve and emit scoped names
%% Org names are either binaries, or they are the special atom global_org
%%

%% This simplifies testing
-spec make_regex() -> re:mp().
make_regex() ->
    {ok, Pattern} = re:compile(?SCOPED_NAME_REGEX),
    Pattern.
%% A scoped name is of the form scope::name. If scope is elided, then we are in the global
%% scope. If there is no scope separator, then it is in the current context.
%%
%% Returns a #sname record, which fully qualifies the name to an org, or the special atom
%% 'global_org'
%%
%% Errors:
%%   ill_formed_name
%%   inappropriate_scoped_name
-spec parse_scoped_name(binary(), boolean(), #context{}) -> #sname{} | {atom(), binary()}.
parse_scoped_name(Name, ScopedOk, Context) ->
    Pattern = make_regex(),
    maybe_parse_scoped_name(Name, Pattern, ScopedOk, Context).

-spec parse_scoped_names([binary()], boolean(), #context{}) -> [#sname{} | {atom(), binary()}].
parse_scoped_names(Names, ScopedOk, Context) ->
    Pattern = make_regex(),
    [ maybe_parse_scoped_name(Name, Pattern, ScopedOk, Context) || Name <- Names ].

-spec maybe_parse_scoped_name(binary(), re:mp(), boolean(), #context{}) ->
                                     #sname{} | {ill_formed_name|inappropriate_scoped_name, binary()}.
maybe_parse_scoped_name(Name, Pattern, ScopedOk, Context) ->
    process_match(re:run(Name, Pattern, [{capture, all, binary}]), Name, Context, ScopedOk).

%% Process the various formats, and expand them
%%
%% If no match, then we have bad name
process_match(nomatch, Name, _Context, _ScopedOk) ->
    {ill_formed_name, Name};
%% If we only match the name, then it is an unscoped name, and we use the org context
process_match({match, [Name, Name]}, Name, Context, _ScopedOk) ->
    set_org_from_context(#sname{base = Name, full = Name}, Context);
%% Anything not a simple, unqualified name should be rejected
process_match({match, _}, Name, _, false) ->
    {inappropriate_scoped_name, Name};
%% If scope is omitted, assume global
process_match({match, [_, <<>>, <<>>, BaseName]}, Name, _, true) ->
    #sname{org = global_org, base = BaseName, full = Name};
%% Fully qualified name
process_match({match, [_, <<>>, OrgName, BaseName]}, Name, _, true) ->
    #sname{org = OrgName, base = BaseName, full = Name}.

set_org_from_context(#sname{} = ScopedName, #context{org_id = OrgId}) when OrgId =/= undefined ->
    ScopedName#sname{org_id = OrgId}.

%%
%% Takes a list of {K, V} pairs with multiple instances of K, and regroups them.
%%
-spec group_by_key([{any(), any()}]) -> [{any(), [any()]}].
group_by_key(L) ->
    Map = lists:foldl(fun({K, V}, Map) ->
                              VL = maps:get(K, Map, []),
                              maps:put(K, [V | VL], Map)
                      end,
                      #{}, L),
    maps:to_list(Map).

%%
%% Expansion of authz ids into scoped names
%% Takes {OrgName, Name} pairs in ScopedNames and returns
%% list of names with scoping metacharacter inserted
-spec render_names_from_org_id(binary(),[{binary(), [binary()]}]) -> [binary()].
render_names_from_org_id(OrgId, ScopedNames) ->
    GroupedScopedNames = group_by_key(ScopedNames),
    Expanded = lists:foldl(fun(E, A) -> render_names_from_org_id_f(OrgId, E, A) end,
                              [], GroupedScopedNames),
    lists:sort(lists:flatten(Expanded)).

%% We are in the same scope, omit qualifier
render_names_from_org_id_f(OrgId, {OrgId, Names}, Expanded) ->
    [Names | Expanded];
%% we are in a different scope, but it's the global scope. Use abbreviated version.
render_names_from_org_id_f(_OrgId, {?GLOBAL_PLACEHOLDER_ORG_ID, Names}, Expanded) ->
    ENames = [ make_name(<<>>, Name) || Name <- Names],
    [ENames | Expanded];
render_names_from_org_id_f(_OrgId, {AnotherOrgId, Names}, Expanded) ->
    %% Design note: we drop missing orgs silently. Org deletion leaks many objects and we must
    %% be robust to that. Thought we will log a warning message to be transparent.
    case org_id_to_name(AnotherOrgId) of
        not_found ->
            lager:warning("Unable to find organization with id '~p'~n", [AnotherOrgId]),
            Expanded;
        OrgName ->
            ENames = [ make_name(OrgName, Name) || Name <- Names ],
            [ENames, Expanded]
    end.

-spec make_name(binary(),binary()) -> <<_:16,_:_*8>>.
make_name(OrgName, Name) ->
    <<OrgName/binary, "::", Name/binary>>.

%%
%% Memorize org id lookup
%%
-spec init_org_name_cache() -> map().
init_org_name_cache() ->
    #{ global_org => ?GLOBAL_PLACEHOLDER_ORG_ID }.

-spec lookup_org_id_cached(binary(), #{}) -> {binary() | not_found, map()}.
lookup_org_id_cached(OrgName, Cache) ->
    case Cache of
        #{OrgName := OrgId} ->
            %% it would be nice to do this in the fun head (OrgId,  #{OrgName := OrgId})
            %% but erlang match and maps don't work that way.
            {OrgId, Cache};
        _ ->
            case chef_sql:fetch_org_metadata(OrgName) of
                not_found ->
                    %% negative results are worth caching
                    {not_found, maps:put(OrgName, not_found, Cache)};
                {OrgId, _AuthzId} ->
                    {OrgId, maps:put(OrgName, OrgId, Cache)}
            end
    end.
