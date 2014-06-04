cjson = require("cjson")
require "routes_test"
require "resolver_test"
require "route_checks_test"

function s_v(data)
  return data or "(nil)"
end

function b_to_s(data)
  return cjson.encode(data)
end

function table_to_string(t)
  return cjson.encode(t)
end

totals = {
  pass = 0,
  fail = 0,
  count = 0
}

function log_result(result)
  local Total = result[2]
  local Fail = result[3]
  local Pass = Total - Fail
  totals.pass = totals.pass + Pass
  totals.fail = totals.fail + Fail
  totals.count = totals.count + Total

  if (Fail > 0) then
    print(result[1] .. ": " .. Pass .. "/" .. Total .. " passed")
  end
end

log_result(test_uri_resolver("api"))
log_result(test_uri_resolver("internal_acct"))
log_result(test_uri_resolver("internal_chef"))
log_result(test_versions())
log_result(test_resolve_upstream())
log_result(test_route_checks())
print(totals.pass .. "/" .. totals.count .. " passed.")

