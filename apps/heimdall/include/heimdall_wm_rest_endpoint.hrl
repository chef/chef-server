-include("heimdall_wm.hrl").

-mixin([{heimdall_wm_base, [content_types_accepted/2,
                            content_types_provided/2,
                            finish_request/2,
                            forbidden/2,
                            malformed_request/2,
                            ping/2,
                            post_is_create/2,
                            service_available/2]}]).

-behavior(heimdall_wm).
-export([allowed_methods/2,
         auth_info/1,
         init/1,
         validate_request/2]).
