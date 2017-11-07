module("route_checks", package.seeall)

-- To preserve the ability to run tests locally, please comment out any
-- chef templating if statements so that the un-rendered portions run in tests.
-- For example:
-- 

local response = {}
response[403] = {}
response[404] = {}
response[503] = {}

-- To add checks for a new response code, first declare it above in the form
-- response[CODE] = {}, then at minimum add a new function response[CODE].default = function(route)


-- To add route-specific checks, add a new function for the given
-- response code in the form of response[CODE].ROUTE_ID = function(route)
-- This function must return "true" if the request shoudl be terminated with "CODE".
--
-- For example to implement "a 404 should occur for the users endpoint on the
-- "acct" route if we are configured to force users endpoint not found":
--
-- response[404].acct = function(org_config, endpoint, object_name)
--   return endpoint == "users" and org_config['force_users_not_found'] == 1
-- end
--

--
-- Default checks -- applied first regardless of endpoint or object-name
-- are handled here.
--
response[503].default = function(route)
   -- Habitat chef server doesn't support 503 mode yet
   if route.org_config["503_mode"] == 1 or
   config.is_route_in_maint_mode(route.route_id) then
      return true
   end
   return false
end

response[404].default = function(route)
   -- route-level darklaunch check:
   -- Habitat chef server doesn't support 404 mode yet
   if config.is_route_darklaunched(route.route_id) then
      return not (route.org_config["dl_" .. route.route_id] == 1)
   end
   return false
end

response[403].default = function(route)
   -- Habitat chef server doesn't support blocked orgs yet
   return route.org_config["org_blocked"] == 1
end

--
-- Endpoint-specific checks from here on down.
--

-- return true if client is posting to "organizations" endpoint but
-- new org creation is disabled
response[503].acct = function(route)
   -- Habitat chef server doesn't support 503 mode yet
  return route.org_name == nil and
         route.endpoint == "organizations" and
         ngx.req.get_method() == "POST" and
         route.org_config["disable_new_orgs"] == 1
end

--
-- Our only public interface
--

-- Run all available checks for the given parameters.  Returns 0
-- if it's clear to proceed, otherwise it returns an http response code
function route_checks.run(route)
  for code, check in pairs(response) do
    if check.default(route) or
       (check[route.route_id] and
        check[route.route_id](route)) then
      return code
    end
  end
  return 0
end


