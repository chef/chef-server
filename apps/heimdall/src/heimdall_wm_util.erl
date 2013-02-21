-module(heimdall_wm_util).

-export([generate_authz_id/0,
         set_created_response/2]).

%% Generate random authz IDs for new objects
generate_authz_id() ->
    % Okay, not so random stub here
    "deadbeefdeadbeefdeadbeefdeadbeef".

scheme(Req) ->
    case wrq:get_req_header("x-forwarded-proto", Req) of
        undefined ->
            case wrq:scheme(Req) of
                https -> "https";
                http -> "http";
                P -> erlang:atom_to_list(P)
            end;
        Proto -> Proto
    end.

port_string(Default) when Default =:= 80; Default =:= 443 ->
    "";
port_string(Port) ->
    [$:|erlang:integer_to_list(Port)].

base_uri(Req) ->
    Scheme = scheme(Req),
    Host = string:join(lists:reverse(wrq:host_tokens(Req)), "."),
    PortString = port_string(wrq:port(Req)),
    Scheme ++ "://" ++ Host ++ PortString.

full_uri(Req, AuthzId) ->
    base_uri(Req) ++ wrq:disp_path(Req) ++ "/" ++ AuthzId.

set_json_body(Req, EjsonData) ->
    Json = jiffy:encode(EjsonData),
    wrq:set_resp_body(Json, Req).

%% Used for all POST /<type> response bodies; always contains ID + URI
set_created_response(Req, AuthzId) ->
    Uri = full_uri(Req, AuthzId),
    Req0 = set_json_body(Req, {[{<<"id">>, list_to_binary(AuthzId)},
                                {<<"uri">>, list_to_binary(Uri)}]}),
    wrq:set_resp_header("Location", Uri, Req0).
