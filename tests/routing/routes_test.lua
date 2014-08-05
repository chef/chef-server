routes = require("routes")

-- Test format:
-- {dest_uri, {org_name_expected, route_id_expected, endpoint_expected, object_name_expected } }

local TEST_ORG = "testorg"
local DEF_ORG =  nil
local endpoint_tests = {
  api = {},
  internal_chef = {},
  internal_acct = {},
}
local shared_chef_tests = {}
local shared_acct_tests = {}

local api_tests = endpoint_tests.api
local int_acct_tests = endpoint_tests.internal_acct
local int_chef_tests = endpoint_tests.internal_chef

-- accounts via organization
table.insert(shared_acct_tests, {"/organizations",                            { DEF_ORG, "acct", "organizations" }})
table.insert(shared_acct_tests, {"/organizations/",                           { DEF_ORG, "acct", "organizations" }})
table.insert(shared_acct_tests, {"/organizations/testorg",                    { TEST_ORG, "acct", "organizations" }})
table.insert(shared_acct_tests, {"/organizations/testorg/",                   { TEST_ORG, "acct", "organizations" }})

for _k, val in pairs{"users", "association_requests"} do
table.insert(shared_acct_tests, {"/organizations/testorg/" .. val,            { TEST_ORG, "acct", val, nil}})
table.insert(shared_acct_tests, {"/organizations/testorg/" .. val .. "/",     { TEST_ORG, "acct", val, nil}})
table.insert(shared_acct_tests, {"/organizations/testorg/" .. val .."/any",   { TEST_ORG, "acct", val, nil}})
end

-- note that the actual endpoint is "/_acl" with support for /_acl/create|update|delete|grant - however our original expression specified
-- our original endpoint accepted "/_acl.*"; we have made it slightly more strict in that it must be
-- /_acl or /_acl/.*
-- alcs are currently darklaunched so their upstream is "acct_erchef"
table.insert(shared_acct_tests, {"/organizations/testorg/nodes/mynode/_acl",  { TEST_ORG, "acct_erchef", "acls"}})
table.insert(shared_acct_tests, {"/organizations/testorg/nodes/mynode/_acl/",  { TEST_ORG, "acct_erchef", "acls"}})
table.insert(shared_acct_tests, {"/organizations/testorg/nodes/mynode/_acl/test",  { TEST_ORG, "acct_erchef", "acls"}})
table.insert(shared_acct_tests, {"/organizations/testorg/nodes/_acl",         { TEST_ORG, "acct_erchef", "acls"}})
table.insert(shared_acct_tests, {"/organizations/testorg/roles/_acl",         { TEST_ORG, "acct_erchef", "acls"}})
-- This goes to account not because _aclextra is valid, but because EVERYTHING is accepted into account for acct internal
table.insert(int_acct_tests, {"/organizations/testorg/nodes/mynode/_aclextra",  { TEST_ORG, "acct", nil}})
-- _aclextra isn't valid, so the correct erchef route should take priority.
table.insert(api_tests, {"/organizations/testorg/nodes/mynode/_aclextra",  { TEST_ORG, "erchef", "nodes", "mynode"}})

-- accounts via /users
table.insert(shared_chef_tests, {"/users",                                    {DEF_ORG, "erchef", "users"}})
table.insert(shared_chef_tests, {"/users/",                                   {DEF_ORG, "erchef", "users"}})
table.insert(shared_chef_tests, {"/users/borg",                               {DEF_ORG, "erchef", "users", "borg"}})
table.insert(shared_chef_tests, {"/users/borg/",                              {DEF_ORG, "erchef", "users", "borg"}})
table.insert(shared_acct_tests, {"/users/borg/association_requests",          {DEF_ORG, "acct", "association_requests", "borg"}})
table.insert(shared_acct_tests, {"/users/borg/association_requests/",         {DEF_ORG, "acct", "association_requests", "borg"}})
table.insert(shared_acct_tests, {"/users/borg/association_requests/abc",      {DEF_ORG, "acct", "association_requests", "borg"}})
table.insert(shared_acct_tests, {"/users/borg//association_requests",         {DEF_ORG, "acct", "association_requests", "borg"}})
table.insert(shared_acct_tests, {"/users/borg//association_requests/",        {DEF_ORG, "acct", "association_requests", "borg"}})
table.insert(shared_acct_tests, {"/users/borg//association_requests/abc",     {DEF_ORG, "acct", "association_requests", "borg"}})
table.insert(shared_acct_tests, {"/users/borg/organizations",                 {DEF_ORG, "acct", "organizations", "borg"}})

-- Different behavior between int-acct and api here, because int-acct will take anything
-- while api requires a valid destiniation. This differs slightly from original behavior, in
-- which api accepted association_requests + 'anythign' without a separator in between
table.insert(api_tests, {"/users/borg//association_requests0abc",     {nil} })
table.insert(api_tests, {"/users/borg/association_requests0abc",      {nil} })
table.insert(int_acct_tests, {"/users/borg//association_requests0abc",     {nil, "acct"} })
table.insert(int_acct_tests, {"/users/borg/association_requests0abc",      {nil, "acct"}})

-- other accounts
table.insert(shared_chef_tests, {"/authenticate_user",  {DEF_ORG, "erchef", "authenticate_user"}})
table.insert(shared_chef_tests, {"/authenticate_user/", {DEF_ORG, "erchef", "authenticate_user"}})

-- These are requests that - for now - account internal is expected to route
-- correctly to erchef, because webui1 hasn't learned not to send requests for
-- migrated componts to chef-internal instead.
-- Migration Phase 3: until webui1 is retired, we'll need to continue to
-- add tests for components migrated acct -> erchef here to ensure they route
-- properly from account internal. Currently just one component, but the
-- form/loop has been kept as a placeholder.
for _k, val in pairs{ "clients" } do
table.insert(int_acct_tests, {"/organizations/testorg/" .. val,           {TEST_ORG, "erchef", val}})
table.insert(int_acct_tests, {"/organizations/testorg/" .. val .. "/",    {TEST_ORG, "erchef", val}})
table.insert(int_acct_tests, {"/organizations/testorg/" .. val .. "/abc", {TEST_ORG, "erchef", val, "abc"}})
table.insert(int_acct_tests, {"/organizations/testorg/" .. val .. "/_acl", {TEST_ORG, "acct_erchef", "acls"}})
end

-- darklaunch account or erchef
-- (none at this time, in Migration Phase 3 we'll have some)

-- erchef
for _k, val in pairs{ "groups", "containers"} do
table.insert(shared_chef_tests, {"/organizations/testorg/" .. val,            { TEST_ORG, "erchef", val, nil}})
table.insert(shared_chef_tests, {"/organizations/testorg/" .. val .. "/",     { TEST_ORG, "erchef", val, nil}})
table.insert(shared_chef_tests, {"/organizations/testorg/" .. val .."/any",   { TEST_ORG, "erchef", val, "any"}})
end

table.insert(shared_chef_tests, {"/license", {DEF_ORG, "erchef", "license"}})

table.insert(shared_chef_tests, {"/organizations/testorg/principals",                  {TEST_ORG, "erchef", "principals"}})
table.insert(shared_chef_tests, {"/organizations/testorg/principals/",                 {TEST_ORG, "erchef", "principals"}})
table.insert(shared_chef_tests, {"/organizations/testorg/principals/asd",              {TEST_ORG, "erchef", "principals", "asd"}})
table.insert(shared_chef_tests, {"/organizations/testorg/principels",                  {nil}})
table.insert(shared_chef_tests, {"/organizations/testorg/nodes/mynode/_identifiers",   {TEST_ORG, "erchef", "nodes", "mynode"}})

table.insert(shared_chef_tests, {"/organizations/testorg/nodes",                      {TEST_ORG, "erchef", "nodes"}})
table.insert(shared_chef_tests, {"/organizations/testorg/nodes/",                     {TEST_ORG, "erchef", "nodes"}})
table.insert(shared_chef_tests, {"/organizations/testorg/environments/envname/nodes",     {TEST_ORG, "erchef", "environments", "envname"}})
table.insert(shared_chef_tests, {"/organizations/testorg/environments/envname/nodes/",     {TEST_ORG, "erchef", "environments", "envname"}})
table.insert(shared_chef_tests, {"/organizations/testorg/environments/envname/node",      {TEST_ORG, "erchef", "environments", "envname"}})
table.insert(shared_chef_tests, {"/organizations/testorg/environments/envname/nodesbad",  {TEST_ORG, "erchef", "environments", "envname"}})
table.insert(shared_chef_tests, {"/organizations/testorg/search",                     {TEST_ORG, "erchef", "search"}})
table.insert(shared_chef_tests, {"/organizations/testorg/search/",                    {TEST_ORG, "erchef", "search"}})
table.insert(shared_chef_tests, {"/organizations/testorg/search/blah",                {TEST_ORG, "erchef", "search"}})
table.insert(shared_chef_tests, {"/organizations/testorg/search?x=1",                 {TEST_ORG, "erchef", "search"}})

for _k, val in pairs{ "cookbooks", "data" ,"roles", "sandboxes", "environments", "clients", "nodes" } do
table.insert(shared_chef_tests, {"/organizations/testorg/" .. val,           {TEST_ORG, "erchef", val}})
table.insert(shared_chef_tests, {"/organizations/testorg/" .. val .. "/",    {TEST_ORG, "erchef", val}})
table.insert(shared_chef_tests, {"/organizations/testorg/" .. val .. "/abc", {TEST_ORG, "erchef", val, "abc"}})
table.insert(shared_chef_tests, {"/organizations/testorg/" .. val .. "/abc/subcomponent", {TEST_ORG, "erchef", val, "abc"}})
table.insert(shared_chef_tests, {"/organizations/testorg/" .. val .. "/b@d!dent", {TEST_ORG, "erchef", val, "b@d!dent"}})
end

-- Default Org
-- table.insert(shared_chef_tests, {"/nodes",                      {TEST_ORG, "erchef", "nodes"}})
-- table.insert(shared_chef_tests, {"/nodes/",                     {TEST_ORG, "erchef", "nodes"}})
-- table.insert(shared_chef_tests, {"/environments/envname/nodes",     {TEST_ORG, "erchef", "environments", "envname"}})
-- table.insert(shared_chef_tests, {"/environments/envname/nodes/",     {TEST_ORG, "erchef", "environments", "envname"}})
-- table.insert(shared_chef_tests, {"/environments/envname/node",      {TEST_ORG, "erchef", "environments", "envname"}})
-- table.insert(shared_chef_tests, {"/environments/envname/nodesbad",  {TEST_ORG, "erchef", "environments", "envname"}})
-- table.insert(shared_chef_tests, {"/search",                     {TEST_ORG, "erchef", "search"}})
-- table.insert(shared_chef_tests, {"/search/",                    {TEST_ORG, "erchef", "search"}})
-- table.insert(shared_chef_tests, {"/search/blah",                {TEST_ORG, "erchef", "search"}})
-- table.insert(shared_chef_tests, {"/search?x=1",                 {TEST_ORG, "erchef", "search"}})

-- for _k, val in pairs{ "cookbooks", "data" ,"roles", "sandboxes", "environments", "clients", "nodes" } do
-- table.insert(shared_chef_tests, {"/" .. val,           {TEST_ORG, "erchef", val}})
-- table.insert(shared_chef_tests, {"/" .. val .. "/",    {TEST_ORG, "erchef", val}})
-- table.insert(shared_chef_tests, {"/" .. val .. "/abc", {TEST_ORG, "erchef", val, "abc"}})
-- table.insert(shared_chef_tests, {"/" .. val .. "/abc/subcomponent", {TEST_ORG, "erchef", val, "abc"}})
-- table.insert(shared_chef_tests, {"/" .. val .. "/b@d!dent", {TEST_ORG, "erchef", val, "b@d!dent"}})
-- end

table.insert(api_tests, {"/organizations/testorg/nodes/_acl",        {TEST_ORG, "acct_erchef", "acls", nil}})

-- Used to return 'erchef' with 'nodes' and 'abc' fo rconsistency with original
-- lb behavior, but fixed it to behave correctly.
table.insert(shared_chef_tests, {"/organizations/testorg/nodesabc", {nil}})

-- Used to return acct w/ 'association_requests' as endpoint in this case, which was incorrect.
-- However these should return  both the org name and the correct upstream (but still no known endpoint),
table.insert(api_tests, {"/organizations/testorg/association_requestsdef",  {nil}})

-- since internal acct accepts everything and should parse what it can
table.insert(int_acct_tests, {"/organizations/testorg/association_requestsdef",  {TEST_ORG, "acct"}})

-- Bad matches should never return a route:
table.insert(shared_chef_tests, {"/organizations/testorg/nOdes/abc/",         {nil}})

-------------------------
-- API tests
-------------------------
-- For API these are invalid due to either to bad identifiers (TestOrg not testorg) or bad paths.
-- Note that internal account will not have these response - see int_acct_tests below.
-- first place...
table.insert(api_tests, {"/organizations/TestOrg",                    {nil}})
table.insert(api_tests, {"/organizations/TestOrg/",                   {nil}})
table.insert(api_tests, {"/organizations/TestOrg/abc",                {nil}})
table.insert(api_tests, {"/bad/path1",                                {nil}})
table.insert(api_tests, {"/organizations/BadOrg/",                    {nil}})
table.insert(api_tests, {"/authenticate_user/bad",                    {nil}})

-- NOTE: This SHOULD fail, however, the rule (in both original and current routing) is that
-- nodes/+ANYTHING goes to erchef - which means we're exposing _identifiers externally today
table.insert(api_tests, {"/organizations/testorg/nodes/mynode/_identifiers",   {TEST_ORG, "erchef", "nodes", "mynode"}})
-------------------------
-- Internal Chef Tests
-------------------------
for k,v in pairs(shared_chef_tests) do
  table.insert(api_tests, k, v)
  table.insert(int_chef_tests, k, v)
end
-- A couple of sanity checks to ensure we're not going to accept things that shoudl go to acct
-- in chef-internal.  These same uris are valid for acct and external.
table.insert(int_chef_tests, {"/organizations/testorg",             {nil}})

-- Note here that we're specifically testing this against 'chef-int' - the  internal
-- chef LB doesn't know about the _acl endpoint and if it receives a rqeuest
-- containing one it will route it as if it weren't present.
table.insert(int_chef_tests, {"/organizations/testorg/nodes/_acl",                 {TEST_ORG, "erchef", "nodes", "_acl"}})
table.insert(int_chef_tests, {"/organizations/testorg/nodes_acl",                  {nil}})

-------------------------
-- Internal Account Tests
-------------------------
for k,v in pairs(shared_acct_tests) do
  table.insert(api_tests, k, v)
  table.insert(int_acct_tests, k, v)
end

-- For internal acct , we accept pretty much everything and route it along - only thing
-- we're really conerned with here is capturing org name and endpoint names.
-- Let's just make sure w're not picking up org name when we shouldn't...
table.insert(int_acct_tests, {"/organizations/TestOrg",                    {DEF_ORG, "acct"}})
table.insert(int_acct_tests, {"/organizations/TestOrg/",                   {DEF_ORG, "acct"}})
table.insert(int_acct_tests, {"/organizations/TestOrg/abc",                {DEF_ORG, "acct"}})

-- And make sure that these get consumed correctly:
table.insert(int_acct_tests, {"/organization",                             {DEF_ORG, "acct"}})
table.insert(int_acct_tests, {"/internal-organization",                    {DEF_ORG, "acct"}})

version_tests = {}
table.insert(version_tests, {"10.0.0",  true})
table.insert(version_tests, {"11.0.0",  true})
table.insert(version_tests, {"0.10.0",  true})
table.insert(version_tests, {"11.0.", true})
table.insert(version_tests, {"0.10.anything", true})
table.insert(version_tests, {"10.0.omething", true})
table.insert(version_tests, {"9.0.0",  false})
table.insert(version_tests, {"0.9.0", false})
table.insert(version_tests, {"12.0.0", false})
table.insert(version_tests, {"11.0", false})

function test_versions()
  local failcount = 0
  for _i, d in pairs(version_tests) do
    local k, v = d[1], d[2]
    local result = routes.is_client_version_valid(k, 10, 11)
    if not (result == v) then
      print("FAIL: " .. k .. " should be " .. b_to_s(v) .. " was " .. b_to_s(result))
      failcount = failcount + 1
    end
  end
  return {"version_checks", #version_tests, failcount}
end

function test_uri_resolver(mode)
  local failcount = 0
  local tests = endpoint_tests[mode]
  local config = {}
  for _k, test in pairs(tests) do
    uri, result = test[1], test[2]
    expected_org, expected_route, expected_endpoint, expected_name = result[1], result[2], result[3], result[4]
    local route = routes.resolve_uri(mode, uri)
    errors = ""
    if not (expected_org == route.org_name) then
      errors = errors .. "org: " .. s_v(expected_org) .. "-" .. s_v(route.org_name) .. " "
    end
    if not (expected_route == route.route_id) then
      errors = errors .. "route: " .. s_v(expected_route) .. "-" .. s_v(route.route_id) .. " "
    end
    if not (expected_endpoint == route.endpoint) then
      errors = errors .. "endpoint: " .. s_v(expected_endpoint) .. "-" .. s_v(route.endpoint) .. " "
    end
    if not (expected_name == route.object_name) then
      errors = errors .. "object name: " .. s_v(expected_name) .. "-" .. s_v(route.object_name) .. " "
    end
    if not (errors == "") then
      failcount = failcount + 1
      print("FAIL: " .. uri .. " -> " .. errors)
    end
  end
  return {"uri_resolver (" .. mode .. ")", #tests, failcount}
end
