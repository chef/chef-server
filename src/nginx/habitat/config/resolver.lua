module("resolver", package.seeall)
local upstream_resolver = { }

-- To preserve the ability to run tests locally, please comment out any
-- chef templating if statements so that the un-rendered portions run in tests.
-- For example:
-- 

-- These names map directly to named upstreams
upstream_resolver.chef = "opscode_chef"
upstream_resolver.erchef = "opscode_erchef"
upstream_resolver.acct = "opscode_account"

-- Resolve an upstream, applying any necessary darklaunch controls.
-- Once we have an upstream name, modify it if the upstream
-- is tarpitted per the configuration.
upstream_resolver.resolve = function(route)
  local upstream = nil
  local route_id = route.route_id
  local resolver_fun = upstream_resolver[route_id .. "_fun"]
  if resolver_fun then
    route_id = resolver_fun(route)
  end
  return upstream_resolver.tarpitify(route_id, route.org_config, route.internal)
end

-- If config names a tarpit for the given route, then
-- alter the upstream name to the appropriate tarpit
upstream_resolver.tarpitify = function(route_id, org_config, internal)
  local upstream = upstream_resolver[route_id]
  -- internal non-API lbs don't tarpit
  if internal then
    return upstream
  end

  tarpitid = org_config["tarpit_" .. route_id]
  if tarpitid then
    return upstream .. "_tarpit" .. tarpitid
  end
  return upstream
end

upstream_resolver.acct_erchef_fun = function(route)
  if route.org_config["couchdb_" .. route.endpoint] == 0 then
    return "erchef"
  end
  return "acct"
end

-- If this is an internal non API vhost , the caller must also ensure that route.internal == true
function resolver.determine_upstream(route)
  return upstream_resolver.resolve(route)
end
