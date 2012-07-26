%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% @author Christopher Maier <cm@opscode.com>
%% @copyright 2012 Opscode, Inc.

-module(chef_data_bag).

-export([
         parse_binary_json/2
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("chef_types.hrl").

-define(VALIDATION_CONSTRAINTS,
        [
         {<<"name">>,             {match, "^[[:alnum:]_\:\.\-]+$"}}
        ]).

%% @doc Convert a binary JSON string representing a Chef data_bag into an EJson-encoded
%% Erlang data structure.
%%
%% @end
-spec parse_binary_json( binary(), create ) ->
                               { ok, ejson_term() }. % or throw
parse_binary_json(Bin, create) ->
    %% TODO: invalid_json will get logged by do_malformed_request, but
    %% currently without any additional information.  Do we want to
    %% emit the JSON we recieved (size limited) or some details of the
    %% parse error from ejson if we can extract it?
    DataBag = ejson:decode(Bin),
    case validate_data_bag(DataBag, create) of
        %% Note: we fill some fields with default values if they are missing
        ok -> {ok, DataBag};
        X -> throw(X)
  end.

validate_data_bag(DataBag, create) ->
    chef_json_validator:validate_json_by_regex_constraints(DataBag,
                                                           ?VALIDATION_CONSTRAINTS).
