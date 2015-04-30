resolver = require("resolver")

local acct_upstream = "opscode_account"
local erchef_upstream = "opscode_erchef"
local reporting_upstream = "opscode_reporting"
local tarpit_name = "TP1"
local tarpit_suffix = "_tarpitTP1"

-- Test format
-- { route_id = "INBOUND_ROUTE_ID",
--   org_config = {} -- any specific configuration that would be loaded for this condition
--   internal = true/false -- true if this is internal LB routing
--   object_name = object name as it would have been resolved, if needed
--   org_name = org name as it would have been resolved, if needed
--   expected = UPSTREAM_NAME
-- }

upstream_tests = {}
table.insert(upstream_tests, { route_id = "erchef",
                               org_config = { },
                               expected = erchef_upstream})
table.insert(upstream_tests, { route_id = "erchef",
                               internal = true,
                               org_config = { tarpit_erchef = tarpit_name },
                               expected = erchef_upstream })
table.insert(upstream_tests, { route_id = "erchef",
                               org_config = { tarpit_erchef = tarpit_name },
                               expected = erchef_upstream .. tarpit_suffix })

table.insert(upstream_tests, { route_id = "acct_erchef",
                               org_config = { couchdb_test = 0, tarpit_erchef = tarpit_name },
                               expected = erchef_upstream .. tarpit_suffix })
table.insert(upstream_tests, { route_id = "acct_erchef",
                               internal = true,
                               org_config = { couchdb_test = 0, tarpit_erchef = tarpit_name },
                               expected = erchef_upstream })
table.insert(upstream_tests, { route_id = "acct_erchef",
                               org_config = { couchdb_test = 1 },
                               expected = acct_upstream })

table.insert(upstream_tests, { route_id = "acct",
                               org_config = {},
                               expected = acct_upstream})
table.insert(upstream_tests, { route_id = "acct",
                               org_config = {couchdb_test = 1, tarpit_acct = tarpit_name},
                               expected =acct_upstream .. tarpit_suffix })
table.insert(upstream_tests, { route_id = "acct",
                               org_config = {couchdb_test = 1},
                               expected = acct_upstream })
table.insert(upstream_tests, { route_id = "acct",
                               org_config = {couchdb_test = 0, tarpit_acct = tarpit_name },
                               expected = acct_upstream .. tarpit_suffix })
table.insert(upstream_tests, { route_id = "acct",
                               internal = true,
                               org_config = {couchdb_test = 0, tarpit_invalid = tarpit_name, },
                               expected = acct_upstream })
table.insert(upstream_tests, { route_id = "acct",
                               org_config = {couchdb_test = 0},
                               expected = acct_upstream })

table.insert(upstream_tests, { route_id = "badvalue", org_config = {}, expected = nil})

function test_resolve_upstream()
  local failcount = 0
  for _i, route in pairs(upstream_tests) do
    route.endpoint = "test"
    -- to simplify inputs we use 'test' endpoint.  corresponding
    -- flag is 'couchdb_test' , which shoudl be present (or not) in config depending on the test
    result = resolver.determine_upstream(route)
    if not (result == route.expected) then
      failcount = failcount + 1
      print("FAIL @ " .. _i .. ": " .. s_v(route.route_id) .. " " .. table_to_string(route.org_config) .. "-> " ..
            s_v(route.expected)  .. "/" .. s_v(result))
    end
  end
  return {"resolve_upstream", #upstream_tests, failcount}
end
