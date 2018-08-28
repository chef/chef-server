%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@chef.io>
%% Copyright 2012-2018 Chef Software, Inc.
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

-module(chef_wm_malformed).

-include("oc_chef_wm.hrl").

-export([
         malformed_request_message/3
        ]).

%% @doc Handle common malformed request tasks with resource-specific callbacks
%%
%% This function does a sanity check on the authn headers (not verifying signing, but does
%% all checks it can without talking to a db).  It also checks for org existence and halts
%% with 404 if the org is not found.  This doesn't strictly belong in malformed_request, but
%% right now all of our resources need this check and end up needing it before
%% resource_exists will get called so we do it here.
%%
%% The caller provides `ValidateFun' which will be given `Req' and `State' as args and
%% should return a `{Req, State}' tuple or throw.  The `ErrorMsgFun' will be called as
%% `ErrorMsgFun(Reason, Req, State)' where `Reason' is the term thrown by `ValidateFun'.
%%
malformed_request_message(bad_clock, Req, State) ->
    {GetHeader, _State1} = chef_wm_util:get_header_fun(Req, State),
    User = case GetHeader(<<"X-Ops-UserId">>) of
               undefined -> <<"">>;
               UID -> UID
           end,
    Msg = iolist_to_binary([<<"Failed to authenticate as ">>, User,
                            <<". Synchronize the clock on your host.">>]),
    {[{<<"error">>, [Msg]}]};
malformed_request_message(bad_sign_desc, _Req, _State) ->
    Msg = <<"Unsupported authentication protocol version">>,
    {[{<<"error">>, [Msg]}]};
malformed_request_message({missing_headers, Missing}, _Req, _State) ->
    Msg = iolist_to_binary([
                            <<"missing required authentication header(s) ">>,
                            bin_str_join(Missing, <<", ">>)]),
    {[{<<"error">>, [Msg]}]};
malformed_request_message({bad_headers, Bad}, _Req, _State) ->
    Msg = iolist_to_binary([
                            <<"bad header(s) ">>,
                            bin_str_join(Bad, <<", ">>)]),
    {[{<<"error">>, [Msg]}]};
malformed_request_message({error, invalid_json}, _Req, _State) ->
    %% in theory, there might be some sort of slightly useful error detail from
    %% chef_json/jiffy, but thus far nothing specific enough to beat out this. Also, would
    %% not passing internal library error messages out to the user when possible.
    {[{<<"error">>, [<<"invalid JSON">>]}]};
malformed_request_message({mismatch, {FieldName, _Pat, _Val}}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary(["Field '", FieldName, "' invalid"])]}]};
malformed_request_message({missing, FieldName}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary(["Field '", FieldName, "' missing"])]}]};
malformed_request_message({both_missing, Field1, _Field2}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary(["Field '", Field1, "' missing"])]}]};
malformed_request_message({client_name_mismatch}, _Req, _State) ->
    {[{<<"error">>, [<<"name and clientname must match">>]}]};
malformed_request_message({bad_client_name, Name, Pattern}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary(["Invalid client name '", Name,
                                       "' using regex: '", Pattern, "'."])]}]};
%% generic invalid name case
malformed_request_message({bad_object_name, Name, Pattern}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary(["Invalid name '", Name,
                                       "' using regex: '", Pattern, "'."])]}]};

%% Not sure if we want to be this specific, or just want to fold this into an 'invalid JSON'
%% case.  At any rate, here it is.
malformed_request_message({bad_string_list, {Field, _Value}}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary(["Field '", Field, "' is not a list of strings"])]}]};
malformed_request_message({bad_ejson_proplist, {Field, _Value}}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary(["Field '", Field, "' is not a hash"])]}]};
malformed_request_message({url_json_name_mismatch, {_UrlName, _Mismatch, Type}}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary([Type, <<" name mismatch.">>])]}]};
malformed_request_message({bad_run_list, {Field, _Value}}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary(["Field '", Field, "' is not a valid run list"])]}]};
malformed_request_message({bad_run_lists, {Field, _Value}}, _Req, _State) ->
    {[{<<"error">>, [iolist_to_binary(["Field '", Field, "' contains invalid run lists"])]}]};
malformed_request_message(invalid_num_versions, _Req, _State) ->
    {[{<<"error">>, [<<"You have requested an invalid number of versions (x >= 0 || 'all')">>]}]};

%% This is used by some custom validation in environments that may (or may not!) be pulled up into ej:valid validations
malformed_request_message({invalid_key, Key}, _Req, _State) ->
    error_envelope([<<"Invalid key ">>, Key, <<" in request body">>]);

malformed_request_message(invalid_json_object, _Req, _State) ->
    error_envelope([<<"Incorrect JSON type for request body">>]);

%% General ej_invalid messages

%% Missing fields
malformed_request_message(#ej_invalid{type=missing,
                                      key=Key
                                     }, _Req, _State) ->
    error_envelope([<<"Field '", Key/binary, "' missing">>]);

%% Exact and string_match failures
malformed_request_message(#ej_invalid{type=Type,
                                      key=Key
                                     }, _Req, _State) when Type =:= exact ;
                                                           Type =:= string_match ->
    % TODO msg is provided here - we should use it if it is.
    error_envelope([<<"Field '", Key/binary, "' invalid">>]);

%% Things that should be hashes, but aren't.
malformed_request_message(#ej_invalid{type=json_type,
                                      key=Key
                                     }, _Req, _State) when Key =:= <<"override_attributes">> ;
                                                           Key =:= <<"default_attributes">> ;
                                                           Key =:= <<"normal">> ;
                                                           Key =:= <<"default">> ;
                                                           Key =:= <<"override">> ;
                                                           Key =:= <<"automatic">> ;
                                                           Key =:= <<"cookbook_versions">> ->
    error_envelope([<<"Field '", Key/binary, "' is not a hash">>]);

%% Entire run list is the wrong type
malformed_request_message(#ej_invalid{type=json_type,
                                     key=Key}, _Req, _State) when Key =:= <<"run_list">> ->
    error_envelope([<<"Field '", Key/binary,"' is not a valid run list">>]);

%% entire env_run_lists is the wrong type
malformed_request_message(#ej_invalid{type=json_type,
                                      key=Key
                                     }, _Req, _State) when Key =:= <<"env_run_lists">> ->
    error_envelope([<<"Field '", Key/binary, "' contains invalid run lists">>]);

%% All other json_type failures
malformed_request_message(#ej_invalid{type=json_type,
                                      key=Key
                                     }, _Req, _State) ->
    error_envelope([<<"Field '", Key/binary, "' invalid">>]);

%% Bogus run list items
%%
%% In the future, we may wish to add more detailed error messages reflecting whether a run
%% list item is simply the wrong type (e.g., a number) or an invalid run list item (e.g.,
%% the string "recipe[").  To do so, we would need to compare the `found_type` and
%% `expected_type` record fields
malformed_request_message(#ej_invalid{type=array_elt,
                                      key=Key}, _Req, _State) when Key =:= <<"run_list">> ->
    error_envelope([<<"Field '", Key/binary, "' is not a valid run list">>]);

malformed_request_message(#ej_invalid{type = object_key,
                                      key = Object,
                                      found = Key}, _Req, _State) ->
    error_envelope([<<"Invalid key '">>, Key, <<"' for ">>, Object]);

malformed_request_message(#ej_invalid{type = object_value,
                                      key = Key
                                     }, _Req, _State) when Key =:= <<"env_run_lists">> ->
    error_envelope([<<"Field '", Key/binary, "' contains invalid run lists">>]);

malformed_request_message(#ej_invalid{type = object_value,
                                      key = Object,
                                      found = Val}, _Req, _State) ->
    error_envelope([<<"Invalid value '">>, Val, <<"' for ">>, Object]);

malformed_request_message(#ej_invalid{type = fun_match, msg = Message}, _Req, _State) ->
    error_envelope([Message]);
malformed_request_message(Reason, Req, #base_state{resource_mod=Mod}=State) ->
    Mod:malformed_request_message(Reason, Req, State).


bin_str_join(L, Sep) ->
    bin_str_join(L, Sep, []).

bin_str_join([H], _Sep, Acc) ->
    lists:reverse([<<"'">>, H, <<"'">>|Acc]);
bin_str_join([H | T], Sep, Acc) ->
    bin_str_join(T, Sep, [Sep, <<"'">>, H, <<"'">> | Acc]).


-spec to_binary( any() ) -> binary().
to_binary(A) when is_atom(A) ->
    atom_to_binary(A, utf8);
to_binary(I) when is_integer(I)->
    list_to_binary(integer_to_list(I));
to_binary(B) when is_binary(B)->
    B;
to_binary(O) ->
    %% Catch-all case
    list_to_binary(io_lib:format("~p", [O])).

-spec binary_message([atom() | string() | binary()]) -> binary().
binary_message(Parts)  ->
    iolist_to_binary([to_binary(P) || P <- Parts]).

-spec error_envelope(binary() | [binary()]) -> ej:json_object().
error_envelope(Parts) when is_list(Parts) ->
    error_envelope(binary_message(Parts));
error_envelope(Message) when is_binary(Message) ->
    chef_wm_util:error_message_envelope(Message).
