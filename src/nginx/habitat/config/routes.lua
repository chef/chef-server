module("routes", package.seeall)

-- To preserve the ability to run tests locally, please comment out any
-- chef templating if statements so that the un-rendered portions run in tests. 
-- For example:
-- 

-- For lpeg syntax, reference www.inf.puc-rio.br/~roberto/lpeg/
local lpeg = require "lpeg"
local match = lpeg.match
local P  = lpeg.P
local B  = lpeg.B
local S  = lpeg.S
local R  = lpeg.R
local C  = lpeg.C
local Ct = lpeg.Ct
local Cg = lpeg.Cg
local Cc = lpeg.Cc

-- create a capture that maps the value of 'name' to capture group "route_id"
local function Crt(name)
  return Cg(Cc(name), "route_id")
end

-- create a capture that maps the captured value of 'exp' to capture group 'endpoint'
local function Cendpoint(exp)
  return Cg(exp, "endpoint")
end

-- match the first argument and set the endpoint to the second argument.
-- useful for when you want to set a route to an endpoint whose name
-- is not contained in the route.
-- for example:
--   Cmatch_and_assign_endpoint(p_users, "associations") * p_sep * p_eol
-- would match /users(/) and set its endpoint to "associations"
local function Cmatch_and_assign_endpoint(p_to_match, endpoint_string)
   return p_to_match * Cendpoint(Cc(endpoint_string))
end

-- Basic identifiers
local p_eol = P(-1)
local p_lower = R"az"
local p_upper = R"AZ"
local p_numeric = R"09"
local p_special = S"_-"
local p_sep = P"/"
local p_dot = P"."

-- Route Components
local p_org = P"organizations"
local p_org_base = P"/organizations"
local p_auth_user = P"authenticate_user"
local p_system_recovery = P"system_recovery"
local p_license = P"license"
local p_api_version = P"server_api_version"
local p_acl = P"/_acl"
local p_search = P"search"
local p_nodes = P"nodes"
local p_cookbooks = P"cookbooks"
local p_data = P"data"
local p_roles = P"roles"
local p_sandboxes = P"sandboxes"
local p_environments = P"environments"
local p_users = P"users"
local p_groups = P"groups"
local p_containers = P"containers"
local p_association_requests = P"association_requests"
local p_clients = P"clients"
local p_runs = P"runs"
local p_principals = P"principals"
local p_internal_organizations_base = P"/internal-organizations"
local p_controls = P"controls"
local p_keys = P"keys"
local p_policyfile = P"policies"
local p_policy_groups = P"policy_groups"
local p_cookbook_artifacts = P"cookbook_artifacts"
local p_universe = P"universe"


-- Composite patterns
local p_maybe_sep = p_sep^-1
local p_trailing_sep = p_maybe_sep * p_eol
local p_org_identifier_char = p_lower + p_numeric + p_special
local p_org_identifier = p_org_identifier_char^1
local p_until_next_separator = (P(1) - p_sep)^1

-- Because we implement different identifiers differently (users & orgs have one set of rules,
-- other objects have different rules) accept anything up to but not including the separator
-- as an identifier sufficient to pass along to the upstream.
local p_identifier = p_until_next_separator

local p_org_prefix = p_org_base * p_sep
local p_named_org = p_org_prefix * Cg(p_org_identifier, "org_name")
local p_named_org_prefix = p_named_org * p_sep
local p_all_until_acl = (P(1) - p_acl)^1
-- Not techncially for routing, this will be used to capture chef version from inbound headers
-- leaving it here to keep all of our grammar in one location.
local p_chef_version = Cg(p_numeric^1, "major") * p_dot * Cg(p_numeric^1, "minor") * p_dot


-- ROUTE CAPTURES
-- These captures will map the constant name to the capture group "route_id".
-- If you're adding a new route, the first step is to define the capture that returns
-- the route name below:

local c_acct = Crt("acct")
local c_erchef = Crt("erchef")
local c_acct_erchef = Crt("acct_erchef")

-- The "valid identifier" capture is used for any object name such
-- as node name, user name, etc.
local c_identifier = Cg(p_identifier, "object_name")

local c_maybe_identifier = (p_sep * c_identifier)^-1

local p_erchef_endpoint = p_cookbooks + p_data + p_roles + p_sandboxes +
                          p_environments + p_clients + p_nodes + p_principals +
                          p_groups + p_containers + p_controls +
                          p_policy_groups + p_policyfile + p_cookbook_artifacts + p_universe


-- endpoints that map directly to erchef
-- If an object identifier is present - as identified with /IDENTIFIER - then
-- capture it. Otherwise -- require end-of-line (in other words, avoid the case
-- of "/nodesSOMEDATA" resolving to 'nodes' endpoint.
local p_erchef_direct = (Cendpoint(p_erchef_endpoint) * ((p_sep * c_identifier) + (p_maybe_sep * p_eol))) +
                        Cendpoint(p_search)

-- users endpoint is currently split between erchef and account.
-- /users, /users/USERNAME/, /authenticate_user, and /system_recover -> erchef
-- /users/USERNAME/organizations, /users/USERNAME/association_requests go to account
local p_erchef_users = (p_sep * Cendpoint(p_users) * p_trailing_sep) +
                       (p_sep * Cendpoint(p_users) * (p_sep * c_identifier)^-1 * p_trailing_sep) +
                       (p_sep * Cendpoint(p_auth_user) * p_trailing_sep) +
                       (p_sep * Cendpoint(p_system_recovery) * p_trailing_sep)

local p_keys_route = ((p_named_org_prefix * p_clients * p_sep * p_identifier * p_sep * Cendpoint(p_keys)) +
                      (p_named_org_prefix * p_users * p_sep * p_identifier * p_sep * Cendpoint(p_keys)) +
                      (p_sep * p_users * p_sep * p_identifier * p_sep * Cendpoint(p_keys))) *
                        ((p_sep * c_identifier)^-1 * p_trailing_sep)


local p_license_route = (p_sep * Cendpoint(p_license) * p_trailing_sep)
local p_server_api_version_route = (p_sep * Cendpoint(p_api_version) * p_trailing_sep)

-- Everything that gets sent to erchef
local p_erchef = p_keys_route +
                 (p_named_org_prefix * p_erchef_direct) +
                 p_erchef_users +
                 p_license_route +
                 p_server_api_version_route

-- erchef routing rules for chef_internal, which includes an additional
-- principals endpoint not exposed via the api rules
local p_erchef_int = p_erchef +
                    (p_named_org_prefix * Cendpoint(p_principals) * p_maybe_sep)

-- /users endpoints for account:
local p_named_user = (p_sep * p_users * p_sep * c_identifier)


                     -- /organizations/:orgname/association_requests(/), custom endpoint for darklaunch
local p_acct_users = (p_named_org_prefix * Cendpoint(p_association_requests) * (p_sep + p_eol)) +
                     -- /organizations/:orgname/users/:username(/), endpoint "associations" for darklaunch
                     (p_named_org_prefix * Cmatch_and_assign_endpoint(p_users, "associations") * p_sep * c_identifier * (p_sep + p_eol)) +
                     -- /organizations/:orgname/users(/)
                     (p_named_org_prefix * Cmatch_and_assign_endpoint(p_users, "associations") * (p_sep + p_eol)) +
                     -- /users/:username/(/)association_requets(/) OR /users/:username/organiztaions(/)
                     p_named_user *
                       -- below rule for backwards compatibility with original:
                       -- /users/BLAH/{0,1}/association_requests
                       -- which permits: /users/BLAH//association_requests
                       ((p_sep * p_maybe_sep * Cendpoint(p_association_requests) * (p_sep + p_eol)) +
                       -- set /users/:username/organizations(/) to endpoint "associations" so we
                       -- can darklaunch it
                       (p_sep * Cmatch_and_assign_endpoint(p_org, "associations") * p_trailing_sep) )


-- /organizations(/:orgname/)
local p_org_endpoint = (p_sep * Cendpoint(p_org) * (p_sep * Cg(p_org_identifier, "org_name"))^-1 * p_trailing_sep)

-- note that the acl endpoint is a special case because it supercedes all others
-- including routes that would otherwise go to erchef.
local p_acl_endpoint = (p_named_org_prefix * p_all_until_acl * Cendpoint(p_acl) * (p_sep + p_eol)) +
                       (p_named_user * Cendpoint(p_acl) * (p_sep + p_eol))


-- Default org endpoints. This is to help with migrating from OSC 11 to Chef Server 12
local p_default_org_endpoints = p_search + p_nodes + p_cookbooks + p_data + p_roles + p_sandboxes +
      p_environments + p_clients + p_runs + p_principals + p_groups + p_containers

-- acct_erchef endpoints that are also routed to internal_acct
local p_internal_acct_erchef_endpoints = p_acct_users + p_org_endpoint

local uri_resolvers = {
  -- Retain ordering to ensure proper eval:
  -- p_acl_endpont must come first because a trailing _acl takes precedence
  -- over any other identifiers which may be in the url.

  api = (p_acl_endpoint * c_acct_erchef) +
        (p_erchef * c_erchef) +
        (p_internal_acct_erchef_endpoints * c_acct_erchef),

  -- This one is easy - everything passes through, though we'll still need to capture components
  -- (org name, object name, endpoint name) where we can, so that post-route hooks can be applied.
  internal_acct =
                  -- as in API, acls come first so that we can ensure webui requests for
                  -- acls get routed correctly to account, even when the underlying object is
                  -- darklaunched to erchef.
                  (p_acl_endpoint * c_acct_erchef) +

                  -- TODO confirm - this should no longer be required.
                  -- Special case: webui1 will still send requests for clients over to us, so
                  -- we have to be able to route migrated endpoints (that used to be acct) to erchef.
                  -- we will need to continue to add migrated acct->erchef endpoints here
                  -- until we either correct or retire webui 1
                  ((p_named_org_prefix * Cendpoint(p_clients) * c_maybe_identifier) * c_erchef) +
                  (p_erchef_users * c_erchef) +
                  (p_internal_acct_erchef_endpoints * c_acct_erchef) +
                  (p_internal_organizations_base * c_acct),

  -- Anything that routes to erchef or reporting is handled here.
  internal_chef = (p_erchef_int * c_erchef)
}

-- Checks if the request is for default orgs
local function needs_default_org(uri)
  return match(p_sep * p_default_org_endpoints, uri)
end

-- caller should bypass this client version check if caller is internal lb
function routes.is_client_version_valid(version, min)
  local res = match(Ct(p_chef_version), version)
  if (res == nil) then
    return false
  end
  local major = tonumber(res.major)
  local minor = tonumber(res.minor)
  if (major or minor) and
     -- Look at the provided arguments, but 'special case' from before we
     -- changed our version string format. ONce we desupport that version,
     -- we will need to make a final update here.
     (major >= min or (major == 0 and minor == 10)) then
    return true
  end
  return false
end

-- Return a table containing:
-- route_id (route id, nil if invalid route)
-- org_name (nil if not found)
-- endpoint (nil if not found)
-- object_name (nil if not found)
function routes.resolve_uri(mode, uri)
  if config.default_org and needs_default_org(uri) then
    uri = '/organizations/' .. config.default_org .. uri
    -- Deliberately do not rewrite uri here, and pass the
    -- original to erchef. Client requests are checksummed
  end
  route = match(Ct(uri_resolvers[mode]), uri)
  if not route then
    route = {}
  end

  -- normalize /_acl endpoint to acls
  if route.endpoint == "/_acl" then
    route.endpoint = "acls"
  end

  return route
end
