-module(chef_wm).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init, 1},
     {validate_request, 3},
     {malformed_request_message, 3},
     {request_type, 0},
     {auth_info, 2}].
