local http_methods = {}
http_methods.GET = function() return "GET" end
http_methods.POST = function() return "POST" end
http_methods.PUT = function() return "PUT" end
http_methods.DELETE = function() return "DELETE " end

-- First stub out nginx stuff that our the route_checks module needs access to.
-- Note that we can't declare this as local since it needs to be visible in route_checks.
ngx = {}
ngx.var = {}
ngx.req = {}
ngx.req.get_method = http_methods.GET
local route_checks = require("route_checks")
local tests = {}

-- Global 'config' is evaluated as well, so stub it out.
config = {}
config.maint_mode_routes = {}
config.darklaunch_routes = {}
config.is_route_in_maint_mode = function(route_id)
  return config.maint_mode_routes[route_id]
end
config.is_route_darklaunched = function(route_id)
  return config.darklaunch_routes[route_id]
end

-- because it's an invalid identier, we can't directly initialze '503_mode' in the table
unavailable_config = {}
unavailable_config["503_mode"] = 1

-- test format
-- {
--    route_id = resolved-ROUTEID
--    endpoint = endpoint name
--    method = "HTTP_METHOD_NAME"
--    ngx_vars = any variables that are expected in ngx.var - typically
--               used to simulate a header, such has ngx.var.http_c_chef_version for x-chef-version
--    expected = EXPECTED_HTTP_RESPONSE_CODE
--


table.insert(tests, { org_config = {disable_new_orgs = 1},
                      route_id = "acct",
                      endpoint = "organizations",
                      method = "GET",
                      expected = 0 } )
table.insert(tests, { org_config = {disable_new_orgs = 1},
                      route_id = "acct",
                      endpoint = "organizations",
                      method = "POST",
                      org_name = nil,
                      expected = 503  } )
table.insert(tests, { org_config = {disable_new_orgs = 1},
                      route_id = "acct",
                      endpoint = "organizations",
                      method = "POST",
                      org_name = "myorg",
                      expected = 0 } )
table.insert(tests, { org_config = {disable_new_orgs = 0},
                      route_id = "acct",
                      endpoint = "organizations",
                      method = "GET",
                      expected = 0 } )
table.insert(tests, { route_id = "acct",
                      endpoint = "organizations",
                      method = "GET",
                      expected = 0 } )
-- Route specific maintenance mode
table.insert(tests, { route_id = "acct",
                      endpoint = "any",
                      maint_mode_override = { acct = true },
                      expected = 503 } )
table.insert(tests, { route_id = "chef",
                      endpoint = "any",
                      maint_mode_override = { acct = true },
                      expected = 0 } )

-- Route specific darklaunch
table.insert(tests, { route_id = "acct",
                      endpoint = "any",
                      darklaunch_override = { acct = true },
                      expected = 404 } )
table.insert(tests, { org_config = { dl_acct = true } ,
                      route_id = "chef",
                      endpoint = "any",
                      darklaunch_override = { acct = true },
                      expected = 0 } )
table.insert(tests, { route_id = "chef",
                      endpoint = "any",
                      darklaunch_override = { acct = true },
                      expected = 0 } )

table.insert(tests, { org_config = unavailable_config, expected = 503, route_id = "any" } )
table.insert(tests, { org_config = { org_blocked = 1 }, expected = 403, route_id = "any" } )
table.insert(tests, { org_config = { }, expected = 0, route_id = "any" } )

function test_route_checks()
  local failcount = 0
  for _k, test in pairs(tests) do
    method = test.method or "GET"
    -- stub out the http method function to match the method to be tested
    ngx.req.get_method = http_methods[method]

    -- Somet things are assumed present
    ngx.var = test.ngx_vars or {}
    test.org_config = test.org_config or {}
    config.maint_mode_routes = test.maint_mode_override or {}
    config.darklaunch_routes = test.darklaunch_override or {}
    result = route_checks.run(test)

    if not (result == test.expected) then
      failcount = failcount + 1
      print("FAIL: " .. table_to_string(test) ..
            " - Result: " .. s_v(test.expected) .. "/" .. s_v(result))
    end
  end
  return {"route_checks", #tests, failcount}
end
