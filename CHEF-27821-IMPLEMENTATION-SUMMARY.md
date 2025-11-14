# CHEF-27821 Implementation Summary

## Overview
This document summarizes the **provisional/experimental** implementation for JIRA ticket CHEF-27821: "Authorization Modifications - DSM Erchef Multi-Tenancy".

**Status:** EXPERIMENTAL - Not production-ready, requires testing and validation
**Branch:** CHEF-27821-experimental (chef-server repo)
**Date:** November 12, 2025
**Implementation Approach:** Local path override for chef_authn library

## Problem Statement

CHEF-27821 requires modifying the Chef Server authentication and authorization logic to support multi-tenancy through a gateway architecture:

- **Gateway** rewrites incoming requests:
  - Modifies `X-Ops-UserId` to include tenant suffix: `userName__{platformTenantUUID}`
  - Potentially modifies request URL to include tenant routing
  - Preserves original values in new headers: `X-Ops-Original-UserId`, `X-Ops-Original-URL`, `X-Ops-Tenant-Id`

- **DSM-erchef** must:
  - Use **original** headers for signature verification (client signed with original values)
  - Support fallback lookup if modified username not found in database
  - Maintain backward compatibility when gateway headers are absent

## Provisional Assumptions

Due to ambiguities in the JIRA ticket specifications, the following provisional answers were used:

1. **Fallback Logic:** "Fall back to X-Ops-Original-UserId if we fail to resolve/verify using X-Ops-UserId"
   - Interpretation: Try fetch_requestors with modified name first
   - If not_found, retry with X-Ops-Original-UserId
   - This handles the case where database hasn't been migrated to tenant-aware usernames yet

2. **Username Mapping Format:** `{userName}__{platformTenantUUID}` (POSTFIX with double underscore)
   - Confirmed from Confluence documentation
   - Gateway appends tenant UUID to username, separated by `__`

3. **URL Header Format:** Assumed to be either full URL (`http://host/path`) or path-only (`/path`)
   - Implementation handles both formats via URL parsing logic

## Implementation Details

### Files Modified

#### 1. `src/oc_erchef/rebar.config`
**Purpose:** Enable local testing of chef_authn changes

**Change:**
```erlang
% Before:
{chef_authn, ".*",
    {git, "https://github.com/chef/chef_authn", {branch, "main"}}},

% After:
{chef_authn, ".*",
    {path, "/home/link/chef_authn"}},
```

**Rationale:** chef_authn is an external library. Path override allows modifying it locally without pushing to remote repository.

---

#### 2. `/home/link/chef_authn/src/chef_authn.erl`

##### Change A: Modify `do_authenticate_user_request` (lines ~599-620)
**Purpose:** Use original headers for signature verification

**Implementation:**
```erlang
do_authenticate_user_request(GetHeader, Method, Path, Body, PublicKey, TimeSkew) ->
    % CHEF-27821: Use X-Ops-Original-UserId for signature verification if present
    % (gateway may rewrite X-Ops-UserId to include tenant suffix)
    UserId = case GetHeader(<<"X-Ops-Original-UserId">>) of
                 undefined -> GetHeader(<<"X-Ops-UserId">>);
                 OriginalUserId -> OriginalUserId
             end,
    
    % CHEF-27821: Use X-Ops-Original-URL for signature verification if present
    % (gateway may rewrite URL to include tenant routing)
    SignaturePath = case GetHeader(<<"X-Ops-Original-URL">>) of
                        undefined -> Path;
                        OriginalUrl -> extract_path_from_url(OriginalUrl)
                    end,
    
    ReqTime = GetHeader(<<"X-Ops-Timestamp">>),
    ContentHash = GetHeader(<<"X-Ops-Content-Hash">>),
    AuthSig = sig_from_headers(GetHeader, 1, []),
    [{algorithm, SignAlgorithm}, {version, SignVersion}] =  validate_headers(GetHeader, TimeSkew),
    BodyHash = hashed_body(Body, {SignAlgorithm, SignVersion}),
    Plain = canonicalize_request(BodyHash, UserId, Method, ReqTime,
                                 SignaturePath, SignAlgorithm, SignVersion, GetHeader),
    verify_sig_or_sigs(Plain, BodyHash, ContentHash, AuthSig, UserId, PublicKey, {SignAlgorithm, SignVersion}).
```

**Key Points:**
- Checks for `X-Ops-Original-UserId` first, falls back to `X-Ops-UserId`
- Checks for `X-Ops-Original-URL` first, falls back to actual `Path`
- Uses extracted values for building canonical string for signature verification
- Maintains backward compatibility when original headers are absent

##### Change B: Add `extract_path_from_url` helper (after line ~288)
**Purpose:** Extract path component from full URL or return path as-is

**Implementation:**
```erlang
%% @doc Extract path component from a full URL or return the path as-is
%% CHEF-27821: Gateway may provide full URL in X-Ops-Original-URL header
%% This function handles both "http://host/path" and "/path" formats
-spec extract_path_from_url(binary()) -> binary().
extract_path_from_url(UrlOrPath) ->
    case binary:match(UrlOrPath, <<"://">>) of
        nomatch ->
            %% No protocol found - assume it's already a path
            UrlOrPath;
        {Pos, _Len} ->
            %% Full URL found - extract path portion
            %% Skip past "://" and find the next "/"
            HostStart = Pos + 3,
            case binary:match(UrlOrPath, <<"/">>, [{scope, {HostStart, byte_size(UrlOrPath) - HostStart}}]) of
                nomatch ->
                    %% No path after host - return root
                    <<"/">>;
                {PathStart, _} ->
                    %% Extract from path start to end
                    binary:part(UrlOrPath, PathStart, byte_size(UrlOrPath) - PathStart)
            end
    end.
```

**Key Points:**
- Handles both `"http://host/path"` and `"/path"` formats
- Uses binary matching for efficient parsing
- Returns `"/"` if URL has no path component

---

#### 3. `src/oc_erchef/apps/oc_chef_wm/src/oc_chef_wm_base.erl`

##### Change A: Modify `authorization_data_extractor` (line ~446)
**Purpose:** Extract path from X-Ops-Original-URL header if present

**Implementation:**
```erlang
-spec authorization_data_extractor(atom(), wm_req(), #base_state{}) -> any().
authorization_data_extractor(path, Req, _State) ->
    %% CHEF-27821: Check for X-Ops-Original-URL header first (set by gateway)
    %% This preserves the original URL for signature verification
    case wrq:get_req_header("x-ops-original-url", Req) of
        undefined ->
            %% No original URL - use actual request path
            iolist_to_binary(wrq:path(Req));
        OriginalUrl ->
            %% Extract path from URL (handles both full URL and path-only)
            iolist_to_binary(extract_path_from_original_url(OriginalUrl))
    end.
```

**Key Points:**
- Called by `verify_request_signature` to extract path for authentication
- Checks for `x-ops-original-url` header first (lowercase, Webmachine convention)
- Falls back to `wrq:path(Req)` if header absent
- Delegates URL parsing to helper function

##### Change B: Add `extract_path_from_original_url` helper (before line ~445)
**Purpose:** URL parsing logic (mirrors chef_authn implementation)

**Implementation:**
```erlang
%% @doc Extract path component from a full URL or return the path as-is
%% CHEF-27821: Gateway may provide full URL in X-Ops-Original-URL header
%% This function handles both "http://host/path" and "/path" formats
extract_path_from_original_url(UrlOrPath) when is_list(UrlOrPath) ->
    extract_path_from_original_url(list_to_binary(UrlOrPath));
extract_path_from_original_url(UrlOrPath) when is_binary(UrlOrPath) ->
    case binary:match(UrlOrPath, <<"://">>) of
        nomatch ->
            %% No protocol found - assume it's already a path
            UrlOrPath;
        {Pos, _Len} ->
            %% Full URL found - extract path portion
            %% Skip past "://" and find the next "/"
            HostStart = Pos + 3,
            case binary:match(UrlOrPath, <<"/">>, [{scope, {HostStart, byte_size(UrlOrPath) - HostStart}}]) of
                nomatch ->
                    %% No path after host - return root
                    <<"/">>;
                {PathStart, _} ->
                    %% Extract from path start to end
                    binary:part(UrlOrPath, PathStart, byte_size(UrlOrPath) - PathStart)
            end
    end.
```

**Key Points:**
- Handles both list and binary input (Webmachine headers can be either)
- Same URL parsing logic as chef_authn version
- Maintains consistency across authentication flow

##### Change C: Modify `verify_request_signature` (lines ~1038-1082)
**Purpose:** Implement fallback logic for username resolution

**Implementation:**
```erlang
verify_request_signature(Req,
                         #base_state{organization_name = OrgName,
                                     organization_guid = OrgId,
                                     auth_skew = AuthSkew,
                                     chef_db_context = DbContext}=State,
                         Extractor) ->
    Name = wrq:get_req_header("x-ops-userid", Req),
    %% CHEF-27821: Try fetching requestor with potentially modified name first
    %% If not found, fallback to original name (gateway may have added tenant suffix)
    {Requestors, FinalName} = case chef_db:fetch_requestors(DbContext, OrgId, Name) of
        not_found ->
            %% Fallback to X-Ops-Original-UserId if present
            case wrq:get_req_header("x-ops-original-userid", Req) of
                undefined ->
                    %% No fallback available
                    {not_found, Name};
                OriginalName ->
                    %% Try with original name (without tenant suffix)
                    case chef_db:fetch_requestors(DbContext, OrgId, OriginalName) of
                        not_found ->
                            %% Still not found - use original name for error message
                            {not_found, OriginalName};
                        Result ->
                            %% Found using original name
                            {Result, OriginalName}
                    end
            end;
        Result ->
            %% Found using modified name
            {Result, Name}
    end,
    
    case Requestors of
        not_found ->
            NotFoundMsg = verify_request_message(user_or_client_not_found, FinalName, OrgName),
            {false, wrq:set_resp_body(chef_json:encode(NotFoundMsg), Req),
             State#base_state{log_msg = {not_found, user_or_client}}};
        {error, no_connections=Error} ->
            Msg = verify_request_message(error_finding_user_or_client, FinalName, OrgName),
            {{halt, 503}, wrq:set_resp_body(chef_json:encode(Msg), Req),
             State#base_state{log_msg = {error_finding_user_or_client, Error}}};
        {error, Error} ->
            Msg = verify_request_message(error_finding_user_or_client, FinalName, OrgName),
            {{halt, 500}, wrq:set_resp_body(chef_json:encode(Msg), Req),
             State#base_state{log_msg = {error_finding_user_or_client, Error}}};
        _ ->
            %% [Rest of authentication logic unchanged]
            PublicKey = select_user_or_webui_key(Req, Requestors),
            Body = body_or_default(Req, <<>>),
            HTTPMethod = method_as_binary(Req),
            Path = Extractor(path, Req, State),
            {GetHeader, State1} = chef_wm_util:get_header_fun(Req, State),
            case chef_authn:authenticate_user_request(GetHeader, HTTPMethod,
                                                      Path, Body, PublicKey,
                                                      AuthSkew) of
                {name, _UserId, Requestor} ->
                    {true, Req, State1#base_state{requestor_id = authz_id(Requestor),
                                                  requestor = Requestor}};
                {no_authn, Reason} ->
                    Msg = verify_request_message(Reason, FinalName, OrgName),
                    Json = chef_json:encode(Msg),
                    Req1 = wrq:set_resp_body(Json, Req),
                    {false, Req1, State1#base_state{log_msg = Reason}}
            end
    end.
```

**Key Points:**
- Two-stage username resolution:
  1. Try with `X-Ops-UserId` (potentially modified by gateway with tenant suffix)
  2. If not_found, try with `X-Ops-Original-UserId` (original username without suffix)
- Tracks `FinalName` for error messages (uses whichever name was attempted)
- Maintains all existing error handling paths
- Only performs fallback when first lookup returns `not_found` (not for database errors)

## Authentication Flow

### Normal Request (No Gateway)
```
Client → DSM-erchef
Headers: X-Ops-UserId: "john"
         X-Ops-Timestamp: "2024-..."
         (no X-Ops-Original-* headers)

Flow:
1. chef_authn reads X-Ops-UserId: "john"
2. chef_authn reads path from request
3. Signature verified using "john" and actual path
4. verify_request_signature calls fetch_requestors(OrgId, "john")
5. Returns user record, authentication succeeds
```

### Gateway Request (Multi-Tenant)
```
Client → Gateway → DSM-erchef
Client sends:
  X-Ops-UserId: "john"
  URL: "/organizations/myorg/nodes"
  (signature based on above)

Gateway rewrites:
  X-Ops-UserId: "john__abc-123-def"  (added tenant UUID)
  X-Ops-Original-UserId: "john"      (preserved original)
  X-Ops-Original-URL: "http://gateway/organizations/myorg/nodes"  (preserved original)
  X-Ops-Tenant-Id: "abc-123-def"     (tenant identifier)
  URL: "/tenants/abc-123-def/organizations/myorg/nodes"  (tenant routing)

Flow:
1. chef_authn reads X-Ops-Original-UserId: "john" (fallback from X-Ops-UserId)
2. chef_authn reads X-Ops-Original-URL, extracts path: "/organizations/myorg/nodes"
3. Signature verified using original "john" and original path
4. verify_request_signature reads X-Ops-UserId: "john__abc-123-def"
5. Calls fetch_requestors(OrgId, "john__abc-123-def")
6. If not_found, reads X-Ops-Original-UserId: "john"
7. Calls fetch_requestors(OrgId, "john") - SUCCESS
8. Returns user record, authentication succeeds
```

## Database Considerations

### User/Client Resolution
- `fetch_requestors(DbContext, OrgId, Name)` queries the `keys_by_name` view
- SQL query: `SELECT ... WHERE (org_id = $1 OR org_id = 'global') AND name = $2 ORDER BY type DESC`
- Returns both users (global org_id) and clients (specific org_id)
- `ORDER BY type DESC` ensures predictable ordering when both exist

### Migration Scenarios
The fallback logic supports these scenarios:

**Scenario 1: Pre-migration (no tenant-aware usernames)**
- Database contains: "john" (original username)
- Gateway sends: X-Ops-UserId="john__abc-123", X-Ops-Original-UserId="john"
- Lookup 1: fetch_requestors("john__abc-123") → not_found
- Lookup 2: fetch_requestors("john") → SUCCESS

**Scenario 2: Post-migration (tenant-aware usernames)**
- Database contains: "john__abc-123" (tenant-aware username)
- Gateway sends: X-Ops-UserId="john__abc-123", X-Ops-Original-UserId="john"
- Lookup 1: fetch_requestors("john__abc-123") → SUCCESS (no fallback needed)

**Scenario 3: No gateway (backward compatibility)**
- Client sends: X-Ops-UserId="john" (no X-Ops-Original-* headers)
- Lookup 1: fetch_requestors("john") → SUCCESS
- Signature verification uses standard headers

## Testing Requirements

### Unit Tests Needed

#### chef_authn Tests (`/home/link/chef_authn/test/`)
1. **extract_path_from_url/1:**
   - Test full URL: `<<"http://example.com/path/to/resource">>` → `<<"/path/to/resource">>`
   - Test path-only: `<<"/path/to/resource">>` → `<<"/path/to/resource">>`
   - Test URL without path: `<<"http://example.com">>` → `<<"/">>`
   - Test URL with query: `<<"http://example.com/path?key=value">>` → `<<"/path?key=value">>`
   - Test HTTPS: `<<"https://example.com/path">>` → `<<"/path">>`

2. **do_authenticate_user_request/6:**
   - Test with X-Ops-Original-UserId present
   - Test with X-Ops-Original-UserId absent (fallback to X-Ops-UserId)
   - Test with X-Ops-Original-URL present (full URL)
   - Test with X-Ops-Original-URL present (path-only)
   - Test with X-Ops-Original-URL absent (fallback to Path)
   - Test signature verification with original headers
   - Test signature verification failure with mismatched headers

#### oc_chef_wm Tests (`src/oc_erchef/apps/oc_chef_wm/test/`)
1. **extract_path_from_original_url/1:**
   - Same URL parsing tests as chef_authn
   - Test with string input (Webmachine header compatibility)

2. **authorization_data_extractor/3:**
   - Test with x-ops-original-url header present
   - Test with x-ops-original-url header absent
   - Test path extraction from full URL in header
   - Test path-only value in header

3. **verify_request_signature/3:**
   - Test successful lookup with X-Ops-UserId (no fallback)
   - Test fallback to X-Ops-Original-UserId when first lookup fails
   - Test no fallback when X-Ops-Original-UserId absent
   - Test error responses use correct username in messages
   - Test database error handling (no_connections, generic errors)
   - Test with both users and clients
   - Mock chef_db:fetch_requestors for controlled testing

### Integration Tests Needed
1. End-to-end authentication flow with gateway headers
2. End-to-end authentication flow without gateway headers (backward compatibility)
3. Performance testing: ensure fallback lookup doesn't significantly impact latency
4. Multi-tenant scenario: multiple tenants with same original username
5. Database migration scenarios: test during and after username migration

## Known Limitations & TODOs

### Provisional Answers Requiring Clarification
1. **Fallback Logic Assumption:** Current implementation falls back to X-Ops-Original-UserId if X-Ops-UserId lookup fails
   - **TODO:** Confirm with product team if this is correct behavior
   - **Alternative:** Always use X-Ops-Original-UserId for database lookup if present

2. **Username Mapping Direction:** Assumed POSTFIX format (`userName__{UUID}`)
   - **TODO:** Verify with team that PREFIX format is not used anywhere

3. **URL Header Format:** Assumed either full URL or path-only
   - **TODO:** Check gateway implementation to confirm actual format
   - **TODO:** Test with real gateway integration

### Security Considerations
1. **Header Validation:** No validation that X-Ops-Original-* headers are trustworthy
   - **TODO:** Add header signature or validation mechanism
   - **TODO:** Restrict which sources can set X-Ops-Original-* headers

2. **Tenant Isolation:** No explicit tenant isolation checks
   - **TODO:** Verify OrgId-based isolation is sufficient for multi-tenancy
   - **TODO:** Add X-Ops-Tenant-Id validation against user/organization mapping

3. **Audit Logging:** Current logging may not capture tenant context
   - **TODO:** Include X-Ops-Tenant-Id in audit logs
   - **TODO:** Log fallback resolution events for troubleshooting

### Performance Considerations
1. **Double Database Lookup:** Fallback mechanism performs two fetch_requestors calls
   - **Impact:** Only affects not_found scenarios (should be rare in production)
   - **TODO:** Consider caching negative lookups to avoid repeated queries

2. **URL Parsing:** Added binary matching operations for URL extraction
   - **Impact:** Minimal (binary operations are efficient in Erlang)
   - **TODO:** Benchmark with production request loads

### Code Maintenance
1. **Duplicated URL Parsing:** Same logic in chef_authn and oc_chef_wm_base
   - **TODO:** Consider extracting to shared utility module
   - **Reason:** chef_authn is external library, oc_chef_wm_base is application-specific

2. **Missing Type Specs:** Some helper functions lack proper -spec attributes
   - **TODO:** Add comprehensive type specifications for all new functions

## Building & Testing

### Setup
```bash
cd /home/link/chef-server-repos/chef-server
git checkout CHEF-27821-experimental
```

### Build oc_erchef
```bash
cd src/oc_erchef
./rebar3 compile
```

### Run Tests
```bash
# Unit tests
./rebar3 eunit

# Integration tests (requires test database setup)
make ct

# Coverage report
./rebar3 cover
```

### Build chef_authn
```bash
cd /home/link/chef_authn
./rebar3 compile
./rebar3 eunit
```

### Clean Build
```bash
# chef-server
cd /home/link/chef-server-repos/chef-server/src/oc_erchef
./rebar3 clean
./rebar3 compile

# chef_authn
cd /home/link/chef_authn
./rebar3 clean
./rebar3 compile
```

## Rollback Plan

### Reverting Changes
```bash
# chef-server repo
cd /home/link/chef-server-repos/chef-server
git checkout main  # or previous stable branch

# chef_authn repo
cd /home/link/chef_authn
git checkout main  # restore original chef_authn
```

### Restoring Dependency
```bash
# Edit rebar.config to restore git dependency
cd /home/link/chef-server-repos/chef-server/src/oc_erchef
# Manually edit rebar.config or:
git checkout HEAD -- rebar.config
./rebar3 clean
./rebar3 compile
```

## Next Steps

### Immediate Actions
1. ✅ Implement provisional changes (COMPLETED)
2. ⏳ Create comprehensive unit tests for all new functions
3. ⏳ Run local builds and tests
4. ⏳ Test with mock gateway headers

### Before Production
1. ⏳ Clarify provisional assumptions with product team
2. ⏳ Security review of header handling
3. ⏳ Performance benchmarking with production-like load
4. ⏳ Integration testing with actual gateway component
5. ⏳ Database migration strategy and testing
6. ⏳ Deployment rollback plan and testing

### Documentation Updates Needed
1. ⏳ Update chef_authn README with multi-tenancy support
2. ⏳ Update Chef Server API documentation with new headers
3. ⏳ Create operator guide for gateway configuration
4. ⏳ Update troubleshooting guide with multi-tenant scenarios
5. ⏳ Add deployment runbook for database migration

## Related Work

### Other CHEF-26206 Subtasks (Not Implemented Here)
- **CHEF-27820:** Username mapping implementation
- **CHEF-27822:** Disabled user handling
- **CHEF-27823:** Group membership changes
- **CHEF-27824:** ACL modifications
- **CHEF-27825:** Testing and validation

### Gateway Component
- Gateway implementation responsible for:
  - Setting X-Ops-Original-* headers
  - Rewriting X-Ops-UserId with tenant suffix
  - URL rewriting for tenant routing
  - Tenant identification and validation

## Glossary

- **Gateway:** Multi-tenancy proxy sitting between clients and DSM-erchef
- **Tenant UUID:** Unique identifier for a platform tenant (e.g., "abc-123-def-456")
- **Username Mapping:** Process of appending tenant UUID to username: `john` → `john__abc-123-def`
- **Original Headers:** Headers set by gateway preserving pre-rewrite values (`X-Ops-Original-UserId`, `X-Ops-Original-URL`)
- **Signature Verification:** Cryptographic validation using client's public key to verify request authenticity
- **Canonical String:** Standardized representation of HTTP request used for signature calculation
- **Requestor:** Database record for authenticated user or client (contains public key)
- **fetch_requestors:** Database query function that retrieves user/client records by name

## Contact & Support

For questions about this implementation:
- JIRA Ticket: https://progresssoftware.atlassian.net/browse/CHEF-27821
- Parent Story: https://progresssoftware.atlassian.net/browse/CHEF-26206
- Implementation: Experimental branch, not production-ready

---

**Document Version:** 1.0
**Last Updated:** November 12, 2025
**Status:** PROVISIONAL - Requires testing, validation, and clarification of assumptions
