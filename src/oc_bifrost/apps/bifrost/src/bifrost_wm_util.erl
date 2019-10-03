-module(bifrost_wm_util).

-include("bifrost_wm.hrl").
-include_lib("ej/include/ej.hrl").
-include_lib("stats_hero/include/stats_hero.hrl").

-export([decode/1,
         encode/1,
         generate_authz_id/0,
         get_requestor/2,
         set_created_response/2,
         set_json_body/2]).

%% Generate random authz IDs for new objects
-spec generate_authz_id() -> auth_id().
generate_authz_id() ->
    lists:flatten([io_lib:format("~4.16.0b", [X]) ||
                      <<X:16>> <= crypto:strong_rand_bytes(16) ]).

%% Extract the requestor from the request headers and return updated base state.
-spec get_requestor(wm_req(), base_state()) -> base_state().
get_requestor(Req, #base_state{reqid = ReqId, superuser_id = SuperuserId} = State) ->
    case wrq:get_req_header("X-Ops-Requesting-Actor-Id", Req) of
        undefined ->
            State;
        Id when Id =:= SuperuserId ->
            % Superuser gets a pass
            State#base_state{requestor_id = superuser};
        Id ->
            case ?SH_TIME(ReqId, bifrost_db, exists, (actor, Id)) of
                true ->
                    State#base_state{requestor_id = Id};
                false ->
                    throw({bad_requestor, Id})
            end
    end.

scheme(Req) ->
    case wrq:get_req_header("x-forwarded-proto", Req) of
        undefined ->
            case wrq:scheme(Req) of
                https -> "https";
                http -> "http"
            end;
        Proto -> Proto
    end.

port_string(Default) when Default =:= 80; Default =:= 443 ->
    "";
port_string(Port) ->
    [$:|erlang:integer_to_list(Port)].

base_uri(Req) ->
    Scheme = scheme(Req),
    Host = string:join(wrq:host_tokens(Req), "."),
    PortString = port_string(wrq:port(Req)),
    Scheme ++ "://" ++ Host ++ PortString.

full_uri(Req) ->
    base_uri(Req) ++ "/" ++ wrq:disp_path(Req).

-spec set_json_body(wm_req(), ej:json_object()) -> wm_req().
set_json_body(Req, EjsonData) ->
    Json = encode(EjsonData),
    wrq:set_resp_body(Json, Req).

%% ALL THE JIFFY in one place.  In case we decide to change the library or
%% something.  LIKE WE DO.
-spec encode(ej:json_object()) -> binary().
encode(EjsonData) ->
    iolist_to_binary(jiffy:encode(EjsonData)).

-spec decode(binary()) -> ej:json_object().
decode(JsonData) ->
    jiffy:decode(JsonData).

%% Used for all POST /<type> response bodies; always contains ID + URI
-spec set_created_response(wm_req(), auth_id()) -> wm_req().
set_created_response(Req, AuthzId) ->
    Uri = full_uri(Req),
    Req0 = set_json_body(Req, {[{<<"id">>, list_to_binary(AuthzId)},
                                {<<"uri">>, list_to_binary(Uri)}]}),
    wrq:set_resp_header("Location", Uri, Req0).
