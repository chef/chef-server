-- Note: these modules are loaded during server init, since they're
-- used in every request:
-- routes = routes.lua
-- config = config.lua
-- resolver = resolver.lua

local mode = ngx.var.mode
local internal = false
local min_version = 10

local remote_addr = ngx.var.remote_addr


-- Before we go any further, check banned IPs
if config.is_addr_banned(remote_addr) then
    ngx.exit(ngx.HTTP_FORBIDDEN)
end

-- If chef-user is missing then it's presumably not a chef client.
local userid = ngx.var.http_x_ops_userid or ""
local uri = ngx.var.uri

-- A couple of early-exit validations that are specific
-- to an api vhost:
if mode == "api" then
  -- If we've defined an override upstream and we don't have a userid, use
  -- the override upstream.
  local override_upstream = ngx.var.add_on_override_upstream
  if userid == "" and override_upstream ~= "" then
    ngx.var.upstream = override_upstream
    return
  end

  -- Internal API does not validate chef version
  if not internal then
    -- Exit early: If they don't have the right chef version, send them packing.
    local version = ngx.var.http_x_chef_version
    if version then
      if not routes.is_client_version_valid(version, min_version) then
        ngx.status = ngx.HTTP_BAD_REQUEST
        ngx.say('{"error": "400 - Bad Request: Chef Client version ' ..
                min_version .. ' or higher required. Your version of Chef is ' ..
                version .. '."}')
        ngx.exit(ngx.HTTP_OK)
      end
    else
      -- the request did not originate with a chef client, we'll give something
      -- more friendly to web browsers.
      return ngx.exec("/index.html")
    end
  end

  -- Exit early: If they don't have a chef user id we don't want them here.
  -- Only API server (both internal and external) verify that user id is present.
  if userid == "" then
    ngx.exit(ngx.HTTP_UNAUTHORIZED)
  end
end


-- global maint mode && address NOT excluded? no can do, muchacho
if config.is_in_maint_mode_for_addr(remote_addr) then
  ngx.exit(ngx.HTTP_SERVICE_UNAVAILABLE)
end

-- Parse the URI to determine that it points to a valid destination
-- and extract necessary components to move forward.
local route = routes.resolve_uri(mode, uri)

-- If we don't have a route identifier it means we  couldn't match the url
-- to a valid path.
if not route.route_id then
  ngx.exit(ngx.HTTP_NOT_FOUND)
end

-- Load restrictions and darklaunch constraints for this org.
-- note that org name may not be valid - we'll use an appropriate default if it's not,
-- such as when the URI is valid but does not contain an org name.
route.org_config = config.org_config(route.org_name, mode)

-- API mode means that internal routing rules do not apply, so only flag as internal if
-- we're not in api mode
if not (mode == "api") then
  route.internal = internal
end

-- All remaining checks are done with the full request context that we've built
-- Any non-zero response indicates an http response code that we must terminate with
-- immediately.
local response = route_checks.run(route)
if (response > 0) then
  ngx.exit(response)
end

-- At long last! Against all odds, they're cleared to move onto
-- their final destination. Let's figure out what that is, pack
-- them up with a shiny new set of darklaunch headers,
-- and send them on their way
ngx.var.upstream = resolver.determine_upstream(route)

local dl_header = ""

for k, v in pairs(route.org_config) do dl_header = dl_header .. k .. "=" .. v .. ";" end
ngx.req.set_header("X-Ops-DarkLaunch", dl_header)


