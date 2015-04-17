#!/usr/bin/env escript

% This simple script will parse the deps section of a rebar.config and spit out
% a ruby hash. Used for automatic dependency loading and linking of erlang projects
main([]) ->
  io:fwrite("Usage: ./parse.es rebar.config-path");
main([FileName|_]) ->
    {ok, Terms} = file:consult(FileName),
    Deps = proplists:get_value(deps, Terms),
    Values = [ {Name, Location}   || {Name, _, Location} <- Deps ],
    Rubified = [term_to_ruby_hash(V) || V <- Values ],
    io:fwrite("{~s}~n", [string:join(Rubified, ",")]).

term_to_ruby_hash({Name, {git, URL}}) ->
    io_lib:format("\"~s\" => { \"url\" => \"~s\", \"ref\" => \"HEAD\" } ", [Name, URL]);
term_to_ruby_hash({Name, {git, URL, ""}}) ->
    io_lib:format("\"~s\" => { \"url\" => \"~s\", \"ref\" => \"HEAD\" } ", [Name, URL]);
term_to_ruby_hash({Name, {git, URL, "master"}}) ->
    io_lib:format("\"~s\" => { \"url\" => \"~s\", \"ref\" => \"origin/master\" } ", [Name, URL]);
term_to_ruby_hash({Name, {git, URL, {tag, Tag} } } )->
    io_lib:format("\"~s\" => { \"url\" => \"~s\", \"ref\" => \"~s\" } ", [Name, URL, Tag]);
term_to_ruby_hash({Name, {git, URL, {branch, Branch} } } )->
    io_lib:format("\"~s\" => { \"url\" => \"~s\", \"ref\" => \"origin/~s\" } ", [Name, URL, Branch]);
term_to_ruby_hash({Name, {git, URL, Ref}}) ->
    io_lib:format("\"~s\" => { \"url\" => \"~s\", \"ref\" => \"~s\" } ", [Name, URL, Ref ]).





