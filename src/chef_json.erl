%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Doug Triggs <doug@opscode.com>
%% @copyright 2012 Opscode, Inc.

-module(chef_json).

-export([
         decode_body/1
        ]).

-include("chef_types.hrl").

-spec decode_body( binary() ) -> ejson_term(). % or throw
%% @doc Decodes JSON body and verifies valid payload type
decode_body(Bin) ->
    Body = ejson:decode(Bin),
    verify_json_type(Body).

verify_json_type({JSON}) when is_list(JSON) ->
    {JSON};
verify_json_type(_) ->
    throw(invalid_json_body).
