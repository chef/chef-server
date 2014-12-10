
-record(state, {authz_ids = {sets:new(), sets:new()}, timer_ref = inactive}).
-define(CLEANUP_TIMEOUT, 10000).
