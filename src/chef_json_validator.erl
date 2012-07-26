%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@opscode.com>
%%
%% Exports a function that accepts a JSON document
%% and a list of regex field/value validators, and
%% validates document content against the provided
%% expressions.  Note that the compiled expressions are not
%% cached; this is module is temporary only until a
%% more long-term solution (which includes caching) is implemented.
%%
%% Most of the internal functions were factored up out of
%% chef_node
%%
%% Copyright 2012 Opscode, Inc. All Rights Reserved.
%%
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


-module(chef_json_validator).

-export([validate_json_by_regex_constraints/2]).

-include("chef_types.hrl").

-type re_regex() :: {'re_pattern', integer(), integer(), binary()}.

validate_json_by_regex_constraints(Json, Constraints) ->
    check_required_fields(Json, Constraints).

%%%
%%% check_field checks various term matches
%%%

%%%
%%% { fieldname: binary() } checks for presence
%%%
check_field(Doc, Name) when is_binary(Name) ->
    case ej:get({Name}, Doc) of
        undefined -> {missing, Name};
        _ -> ok
    end;

%%% Requires the key to be present and to match a regexp
%%% { fieldname: binary,
%%%   {'match',
%%%    fieldre: re:compile compatible regexp}}
%%%
check_field(Doc, {Name,{match, Regex}}) when is_binary(Name) ->
    case ej:get({Name}, Doc) of
        undefined -> {missing, Name};
        F ->
            %% We should cache the compiled regexps somehow....
            {ok, ReComp} = re:compile(Regex),
            case re:run(F, ReComp) of
                {match, _} -> ok;
                nomatch -> {mismatch, {Name, Regex, F}}
            end
    end;
%%% The key is optional, but if it is present it must match a regexp
%%% { fieldname: binary,
%%%   {'match_if_exists',
%%%    fieldre: re:compile compatible regexp}}
%%%
check_field(Doc, {Name,{match_if_exists, Regex}}) when is_binary(Name) ->
    case ej:get({Name}, Doc) of
        undefined -> ok;
        F ->
          %%% We should cache the compiled regexps somehow....
          {ok, ReComp} = re:compile(Regex),
          case re:run(F, ReComp) of
              {match, _} -> ok;
              nomatch -> {mismatch, {Name, Regex, F}}
          end
    end;


check_field(Doc, {Name, is_ejson_proplist}) ->
    case ej:get({Name}, Doc) of
        undefined -> {missing, Name};
        ?EMPTY_EJSON_HASH -> ok;
        {X} when is_list(X) ->
            ok; % TODO Make this stricter!
        X -> {bad_ejson_proplist, {Name, X}}
    end;

%% Checks that the given field is a list of binary strings.
%%
check_field(Doc, {Name, is_string_list}) ->
    case ej:get({Name}, Doc) of
        undefined ->
            {missing, Name};
        L ->
            case is_list_of_binary(L) of
                true ->
                    ok;
                false ->
                    {bad_string_list, {Name, L}}
            end
    end;

%% Check that the given field is a list of valid runlist entries
check_field(Doc, {Name, is_run_list}) ->
    case ej:get({Name}, Doc) of
        undefined ->
            {missing, Name};
        L ->
            case is_run_list(L) of
                true ->
                    ok;
                false ->
                    {bad_run_list, {Name, L}}
            end
    end;

%% Check that a map of environment -> runlist contains valid run lists
check_field(Doc, {Name, is_run_list_map}) ->
    case ej:get({Name}, Doc) of
        undefined ->
            {missing, Name};
        ?EMPTY_EJSON_HASH -> ok;
        {L} ->
            case is_env_run_lists(L) of
                true ->
                    ok;
                false ->
                    {bad_run_lists, {Name, L}}
            end;
        X ->
            {bad_run_lists, {Name, X}}
    end.

%%%
%%% Requires the key to be present and to match a regexp
%%%
check_required_fields(_Doc, []) -> ok;
check_required_fields(Doc, [FieldDesc|Remaining]) ->
    case check_field(Doc, FieldDesc) of
        ok -> check_required_fields(Doc, Remaining);
        X -> X
    end.

%

is_list_of_binary([]) ->
    true;
is_list_of_binary([H|T]) when is_binary(H) ->
    is_list_of_binary(T);
is_list_of_binary([_|_]) ->
    false;
is_list_of_binary(_NotAList) ->
    false.

%% @doc Given a Chef run list (as a list of binary strings),
%% determines if all entries are valid run list items.
-spec is_run_list([binary()]) -> boolean().
is_run_list([]) ->
    true;
is_run_list([H|T]) when is_binary(H)->
    case valid_run_list_item(H) of
        true ->
            is_run_list(T);
        false ->
            false
    end;
is_run_list([H|_]) when not is_binary(H) ->
    false;
is_run_list(_NotAList) ->
    false.

%% @doc Abstracts the actual task of matching a regular expression to
%% a binary string.
-spec matches(binary(), re_regex()) -> boolean().
matches(Item, Regexp) when is_binary(Item) andalso
                          %% Wanted to write
                          %% is_record(Regexp, re_pattern)
                          %% but it doesn't work
                          is_tuple(Regexp) andalso
                          element(1, Regexp) =:= re_pattern ->
    case re:run(binary_to_list(Item), Regexp, [{capture, none}]) of
        match ->
            true;
        nomatch ->
            false
    end.

%% @doc Given an item from a Chef run list, determines if it is
%% syntactically correct, taking into account the various types of
%% items that are allowed in a run list.
-spec valid_run_list_item(binary()) -> boolean().
valid_run_list_item(<<"recipe[", _Rest/binary>> = QualifiedRecipe) ->
    {RegExp, _Msg} = chef_regex:regex_for(qualified_recipe),
    matches(QualifiedRecipe, RegExp);
valid_run_list_item(<<"role[", _Rest/binary>> = Role) ->
    {RegExp, _Msg} = chef_regex:regex_for(qualified_role),
    matches(Role, RegExp);
valid_run_list_item(UnqualifiedRecipe) when is_binary(UnqualifiedRecipe) ->
    {RegExp, _Msg} = chef_regex:regex_for(unqualified_recipe),
    matches(UnqualifiedRecipe, RegExp).

%% @doc Given a list of {Env, RunList} tuples, verify that all RunLists are
%% valid
%% @end
%% TODO: Validate that all environment names are valid as well
%% (syntactically; not checking for environment existence yet)
-spec is_env_run_lists([{Env::binary(), [binary()]}]) -> boolean().
is_env_run_lists([]) ->
    true;
is_env_run_lists([{Env, MaybeRunList}|T]) when is_binary(Env)->
    case is_run_list(MaybeRunList) of
        true ->
            is_env_run_lists(T);
        false ->
            false
    end;
is_env_run_lists(_) ->
    false.
