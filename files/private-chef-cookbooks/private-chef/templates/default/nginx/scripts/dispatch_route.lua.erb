-- This is for use with the internal /_routes endpoints
local cjson = require("cjson")
cjson.encode_keep_buffer(false)
args = ngx.req.get_uri_args()

-- Let's go ahead pull the meat out of the url - easy, since we're guaranteed that our URI
-- is prefixed with "/_route/"
local uri = string.sub(ngx.var.uri, 8)
local route = routes.resolve_uri("api", uri)
if not route.route_id then
  ngx.say('{"error": "no route exists for the uri ' .. uri .. '"}')
  ngx.exit(ngx.HTTP_OK)
end

route.org_config = config.org_config(route.org_name)
upstream = resolver.determine_upstream(route)
--
-- construct a table with response data, then serialize to json
local json = {}
-- Start out with the basics
json['org_name'] = route.org_name
json['upstream_target'] = upstream
json['uri'] = uri
json['route'] = route.route_id
json['endpoint'] = route.endpoint
json['object_name'] = route.object_name or ""
json['config'] = {}
json['config']['merged'] = route.org_config

-- debug config - include merged and default/org/override config.
if args['all'] == "1" then
  local raw = config.raw_org_config(org_name)
  json['config']['default'] = raw[1]
  json['config']['org'] = raw[2]
  json['config']['override'] = raw[3]
end

-- Send it back and done.
ngx.say(cjson.encode(json))
ngx.exit(ngx.HTTP_OK)
