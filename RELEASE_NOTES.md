# Chef Infra Server Release Notes

## Version 15.10.68 (2025-09-30)

### Security Updates

**PostgreSQL Security Fix - CVE-2025-8713 (High Severity - CVSS 8.8)**

This release addresses a critical security vulnerability in PostgreSQL:

- **Issue**: PostgreSQL optimizer statistics vulnerability allowing unauthorized data access
- **Impact**: Users could bypass view access control lists (ACLs) and row security policies 
- **Fix**: Updated PostgreSQL from version 13.21 to 13.22
- **Affected Components**: All Chef Infra Server installations using embedded PostgreSQL
- **Action Required**: Upgrade to Chef Infra Server 15.10.68 to receive the security fix

**Technical Details:**
- CVE-2025-8713 allowed crafting malicious operators to access statistics data (histograms, most-common-values lists) 
- The vulnerability could bypass security policies in partitioned tables and table inheritance hierarchies
- This fix is particularly important for multi-tenant Chef Server environments

See [Chef Infra Server Release Notes](https://docs.chef.io/release_notes_server) for the complete list of product release notes.
