-module(chef_wm).

-include("chef_wm.hrl").

-type http_verb() :: 'GET' | 'PUT' | 'POST' | 'DELETE' | 'HEAD' | 'OPTIONS'.
-type base_state() :: #base_state{}.
-type container_name() :: cookbook |
                          data |
                          node |
                          role |
                          sandbox.
-callback init(list()) ->
    {ok, base_state()}.

-callback validate_request(http_verb(), wm_req(), base_state()) ->
    {wm_req(), base_state()}.

-callback malformed_request_message(term(), wm_req(), base_state()) ->
    term().                                     % return is really EJSON

-callback request_type() ->
    string().

-callback auth_info(wm_req(), base_state()) ->
    {{halt, non_neg_integer()}, wm_req(), base_state()} |
    {{create_in_container, container_name()}, wm_req(), base_state()} |
    {{container, container_name()}, wm_req(), base_state()} |
    {{object, object_id()}, wm_req(), base_state()}.
