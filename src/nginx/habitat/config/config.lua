-- this script exposes get_org_config which returns an array of
-- darklaunch configuration maps retrieved from redis
module("config", package.seeall)

-- NOTE: assumption that the following is defined globally:
-- redis = require("resty/redis")

local redis_password = os.getenv("REDIS_PASSWORD")

local failsafe_config = {}
failsafe_config["dl_default"] = {}
failsafe_config["dl_default"][1] = "503_mode"
failsafe_config["dl_default"][2] = true

-- Client must set these via set_ban_refresh_interval
-- and set_maint_refresh_interval.
maint_refresh_interval = nil
ban_refresh_interval = nil
default_org = false

local function b_to_i(v)
  -- these values are coming back as a string from redis. Handle both scenarios
  -- so that we don't break if our redis interface gets smarter down the road.
  if v == "true" or v == true then
    return 1
  elseif v == "false" or v == false then
    return 0
  else
    return v -- preserve string values
  end
end

local function array_to_hash(t)
  local h = {}
  for i = 1, #t, 2 do
    h[t[i]] = t[i + 1]
  end
  return h
end

local function auth_not_required(err)
  -- conservatively try to determine if auth is not required.
  -- if auth isn't required, we can safely ignore an error from
  -- the AUTH command
  return err == "ERR Client sent AUTH, but no password is set"
end

local function connect_redis()
   local red = redis:new()
   red:set_timeout(1000)
   local ok, err = red:connect("127.0.0.1", 16379)

   if not ok then
      ngx.log(ngx.ERR, "failed to connect redis: ", err)
   end

   if redis_password == nil then
      ngx.log(ngx.ERR, "REDIS_PASSWORD not found in the environment")
      ok = false
   else
      local ok, err = red:auth(redis_password)
      if not ok then
         if auth_not_required(err) then
            ok = true
         else
            ngx.log(ngx.ERR, "failed to authenticate to redis: ", err)
        end
      end
   end
   return ok, red
end

local function close_redis(red)
  local ok, err = red:set_keepalive(2000, 250)
  if not ok then
    ngx.log(ngx.ERR, "Failed to set keepalive: " .. err)
  end
end

-- Get configuration information for default/orgname/override in pipelined fashion
-- return failsafe defaults if redis error occurs otherwise an array of results.
local function redis_pipelined_get_config(red, orgname)
  red:init_pipeline()
  red:hgetall("dl_default")
  red:hgetall("dl_org_" .. orgname)
  red:hgetall("dl_override")
  local results, err = red:commit_pipeline()
  if not results then
    ngx.log(ngx.ERR, "failed to commit the pipelined requests: " .. err )
    results = failsafe_config
  end
  return results
end

local function redis_fetch_set(set_name)
  local result = {}
  local ok, red = connect_redis()
  if ok then
    result, err = red:smembers(set_name);
    if err then
      ngx.log(ngx.ERR, "Redis read error retrieving " .. set_name .. ": " .. err);
      ok = false
    else
      close_redis(red)
    end
  end
  return ok, result
end

-- Connect to redis and retrieve configuration for this org.
local function get_org_config(orgname)
  local ok, red = connect_redis()
  if ok then
    results = redis_pipelined_get_config(red, orgname)
    close_redis(red)
  else
    results = failsafe_config
  end
  return results
end

-- Examines the shared_dict provided and determine if it needs updating
-- based on expiry time.  If so, it clears all entries and refreshes
-- the dict data from redis
local function refresh_expiring_set(shared_dict, name, interval)
  local updated_at = shared_dict:get("updated_at");
  if updated_at == nil or (ngx.now() - updated_at) >= interval then
    ok, updated_data = redis_fetch_set(name)
    if ok then
       shared_dict:flush_all();
       for index, key in ipairs(updated_data) do
         shared_dict:set(key, true);
       end
       shared_dict:set("updated_at", ngx.now());
    end
  end
end

-- return true if our systems are in maintenance mode
-- and the remote address is not excluded from maintenance mode
-- will refresh maint-mode state and whitelisted IPs periodically.
function config.is_in_maint_mode_for_addr(remote_addr)
  local maint = ngx.shared.maint_data
  refresh_expiring_set(maint, "maint_data", maint_refresh_interval)
  return maint:get("maint_mode")
end

-- This does not attempt to refresh the maintenance mode data, as it's
-- intended to be invoked only after main data has been loaded/refreshed earlier int he flow
function config.is_route_in_maint_mode(route_id)
  local maint = ngx.shared.maint_data
  return maint:get("maint_mode_" .. route_id)
end

function config.is_route_darklaunched(route_id)
  local maint = ngx.shared.maint_data
  return maint:get("dl_" .. route_id)
end

-- return true if the given address is maintenance mode
-- whitelist
function config.is_addr_whitelisted(component, remote_addr)
  return false
end


function config.is_addr_banned(remote_addr)
  return false
end

-- Get 'raw' org configuration - nested array of darklaunch rules
function config.raw_org_config(org)
  local results = get_org_config(org)
  local c = {}
  local i = 1
  for index, result in pairs(results) do
    c[i] = {}
    for k, v in pairs(array_to_hash(result)) do c[i][k] = v end
    i = i + 1
  end
  return c
end

-- Get org configuration and darklaunch rules
function config.org_config(org)
  local results = get_org_config(org or "_OC_INTERNAL_NO_ORG")
  -- merge results into a single table, with later key values overwriting earlier ones.
  local org_config = {}
  for index, result in pairs(results) do
    for k, v in pairs(array_to_hash(result)) do org_config[k] = b_to_i(v) end
  end
  return org_config
end

function set_ban_refresh_interval(interval)
  ban_refresh_interval = interval
end

function set_maint_refresh_interval(interval)
  maint_refresh_interval = interval
end

function set_default_org(org)
  default_org = org
end
