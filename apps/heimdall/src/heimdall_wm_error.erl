-module(heimdall_wm_error).

-include("heimdall_wm.hrl").

-export([set_db_exception/3,
         set_malformed_request/3]).

%% Sets the error message in the body and returns the return tuple to malformed
%% request (which could contain true or {halt, XXX} if some other return code is
%% appropriate.
set_malformed_request(Req, State, Error) ->
    Msg = malformed_request_message(Error, Req, State),
    {true, heimdall_wm_util:set_json_body(Req, Msg), State}.

malformed_request_message(missing_requestor, _Req, _State) ->
    {[{<<"error">>, <<"must specify a requesting actor id">>}]};
malformed_request_message({bad_requestor, Id}, _Req, _State) ->
    {[{<<"error">>, iolist_to_binary([<<"requesting actor id of '">>, Id,
                                      <<"' does not exist">>])}]};
malformed_request_message(Why, _Req, _State) ->
    error({unexpected_error_message, Why}).

%% Sets the error message in the body and returns the return tuple to malformed
%% request (which should contain {halt, XXX} with whatever return code is
%% appropriate.

%% So far, all errors are unexpected, we bail on everything:
set_db_exception(_Req, _State, Error) ->
    error({unexpected_error_message, Error}).
