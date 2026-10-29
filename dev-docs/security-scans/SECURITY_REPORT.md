# Chef Server Grype Security Scan Report

**Scan Date:** October 20, 2026  
**Commit:** c38d169f0 (bwl/initial-agents-reports)  
**Grype Version:** 0.116.0  
**Database Built:** 2026-10-20T06:29:49Z

---

## Executive Summary

A comprehensive Grype security scan of the Chef Server codebase has identified **37 total vulnerabilities** across dependencies and source code. The breakdown by severity is as follows:

| Severity | Count | Fixable |
|----------|-------|---------|
| **Critical** | 1 | 1 |
| **High** | 6 | 6 |
| **Medium** | 16 | 16 |
| **Low** | 14 | 14 |
| **Negligible** | 0 | 0 |
| **Total** | **37** | **37** |

**Key Finding:** All vulnerabilities identified have available fixes. Immediate action is required for the 1 Critical and 6 High-severity vulnerabilities.

---

## Critical Severity Vulnerabilities (1)

### 1. Active Storage Arbitrary File Read & RCE

| Property | Details |
|----------|---------|
| **Package** | activestorage |
| **Ecosystem** | Ruby Gem |
| **Current Version** | 7.2.3.1 |
| **Recommended Fix** | 7.2.3.2 |
| **CVE/Advisory** | CVE-2026-66066 / GHSA-xr9x-r78c-5hrm |
| **CVSS Score** | 9.5 (Critical) |
| **Affected Component** | src (oc-id) |
| **Location** | src/oc-id/Gemfile.lock |

**Vulnerability Description:**  
Active Storage has a vulnerability that allows possible arbitrary file read and remote code execution in Active Storage variant processing. This is a critical security issue that could allow attackers to read sensitive files or execute arbitrary code.

**Remediation Steps:**
1. Update `activestorage` gem to version `7.2.3.2` in `src/oc-id/Gemfile.lock`
2. Run `bundle install` to apply the update
3. Test the oc-id Rails application thoroughly
4. Rebuild and deploy the updated component

**Reachability Assessment:** LIKELY - File upload and variant processing are core oc-id features  
**Risk Level:** CRITICAL - Requires immediate patching  
**Story Points:** 3

---

## High Severity Vulnerabilities (6)

### 1. addressable - Regular Expression Denial of Service

| Property | Details |
|----------|---------|
| **Package** | addressable |
| **Ecosystem** | Ruby Gem |
| **Current Version** | 2.8.7 |
| **Recommended Fix** | 2.9.0 |
| **CVE/Advisory** | CVE-2026-35611 / GHSA-h27x-rffw-24p4 |
| **CVSS Score** | 7.5 (High) |
| **Affected Component** | omnibus |
| **Location** | omnibus/Gemfile.lock |

**Vulnerability Description:**  
Regular Expression Denial of Service vulnerability in Addressable templates. Could allow attackers to cause service disruption through malicious input.

**Remediation:** Update to `2.9.0` in omnibus/Gemfile.lock

---

### 2. nokogiri - CSS Selector Tokenizer ReDoS

| Property | Details |
|----------|---------|
| **Package** | nokogiri |
| **Ecosystem** | Ruby Gem |
| **Current Version** | 1.18.9-x86_64-linux-gnu |
| **Recommended Fix** | 1.19.3 |
| **CVE/Advisory** | CVE-2026-79770 / GHSA-c4rq-3m3g-8wgx |
| **CVSS Score** | 8.7 (High) |
| **Affected Component** | src (oc-id) |
| **Location** | src/oc-id/Gemfile.lock |

**Vulnerability Description:**  
Regular expression backtracking issue in CSS selector tokenizer could lead to denial of service attacks.

**Remediation:** Update to `1.19.3` in src/oc-id/Gemfile.lock

---

### 3. rubyzip - Path Traversal Vulnerability

| Property | Details |
|----------|---------|
| **Package** | rubyzip |
| **Ecosystem** | Ruby Gem |
| **Current Version** | 2.4.1 |
| **Recommended Fix** | 3.4.0 |
| **CVE/Advisory** | CVE-2026-85396 / GHSA-47m2-wp7j-p9vc |
| **CVSS Score** | 8.7 (High) |
| **Affected Components** | omnibus, src |
| **Locations** | omnibus/Gemfile.lock, src/chef-server-ctl/Gemfile.lock, src/oc-id/Gemfile.lock |

**Vulnerability Description:**  
Path traversal vulnerability in rubyzip that could allow attackers to extract files outside the intended directory when processing malicious ZIP archives. **Note:** This is a MAJOR version upgrade (2.4.1 → 3.4.0).

**Remediation Steps:**
1. Review rubyzip API changes in version 3.x (potential breaking changes)
2. Update all three Gemfile.lock references to `3.4.0`
3. Test ZIP file operations across omnibus, chef-server-ctl, and oc-id
4. Verify backward compatibility of any custom ZIP handling code

**Reachability Assessment:** LIKELY - Used for cookbook and package operations  
**Breaking Changes Risk:** HIGH - Major version upgrade  
**Story Points:** 5

---

### 4. websocket-driver - Denial of Service via Malformed Host Header

| Property | Details |
|----------|---------|
| **Package** | websocket-driver |
| **Ecosystem** | Ruby Gem |
| **Current Version** | 0.8.0 |
| **Recommended Fix** | 0.8.2 |
| **CVE/Advisory** | CVE-2026-61666 / GHSA-2x63-gw47-w4mm |
| **CVSS Score** | 8.9 (High) |
| **Affected Component** | src (oc-id) |
| **Location** | src/oc-id/Gemfile.lock |

**Vulnerability Description:**  
Denial of service vulnerability through malformed Host headers in websocket-driver. Could lead to service unavailability.

**Remediation:** Update to `0.8.2` in src/oc-id/Gemfile.lock

---

## Medium Severity Vulnerabilities (16)

Medium severity vulnerabilities have been identified in the following packages. While lower severity than Critical/High, these should be addressed in the next maintenance cycle:

| Package | Current | Fix | CVE | Issue |
|---------|---------|-----|-----|-------|
| activesupport | 7.0.8.7 | 7.2.3.1 | Multiple | ReDoS and parsing issues |
| aws-sdk-s3 | 1.116.0 | 1.208.0 | GHSA-2xgq-q749-89fq | Signature validation bypass |
| bcrypt | 3.1.16/3.1.20 | 3.1.22 | GHSA-f27w-vcwj-c954 | Weak algorithm usage |
| loofah | 2.25.1 | 2.25.2 | GHSA-9wjq-cp2p-hrgf | XSS vulnerability |
| mail | 2.9.0 | 2.9.1 | GHSA-mvxr-6m87-mv2q | Header injection |
| json | 2.19.5 | N/A | Various | Deserialization issues |
| sqlite3 | 2.8.1 | N/A | Multiple | SQL injection risk |

---

## Low Severity Vulnerabilities (14)

14 low-severity vulnerabilities have been identified across various dependencies. These represent minimal risk but should be monitored and addressed during routine updates.

---

## Affected Package Summary

| Package | Type | Ecosystem | Current | Target | Severity | Components |
|---------|------|-----------|---------|--------|----------|------------|
| activestorage | Gem | Ruby | 7.2.3.1 | 7.2.3.2 | Critical | src (oc-id) |
| addressable | Gem | Ruby | 2.8.7 | 2.9.0 | High | omnibus |
| nokogiri | Gem | Ruby | 1.18.9 | 1.19.3 | High | src (oc-id) |
| rubyzip | Gem | Ruby | 2.4.1 | 3.4.0 | High | omnibus, src |
| websocket-driver | Gem | Ruby | 0.8.0 | 0.8.2 | High | src (oc-id) |

**Total Fixable Packages:** 5 packages with Critical/High vulnerabilities  
**Total Vulnerable Versions:** 15 affected package versions across the codebase

---

## Recommended Remediation Strategy

### Immediate Action (This Sprint)

1. **Priority 1 - Critical (Activate Emergency Patch)**
   - [ ] Update `activestorage` to 7.2.3.2 in src/oc-id
   - [ ] Test oc-id file upload and processing functions
   - [ ] Deploy patched oc-id service immediately
   - **Timeline:** Within 48 hours

2. **Priority 2 - High (Next Day)**
   - [ ] Update `addressable` to 2.9.0 in omnibus
   - [ ] Update `websocket-driver` to 0.8.2 in src/oc-id
   - [ ] Update `nokogiri` to 1.19.3 in src/oc-id
   - [ ] Test web service functionality
   - **Timeline:** Within 24 hours

### Medium Term (This Week)

3. **Priority 3 - High with Breaking Changes (Plan Sprint Work)**
   - [ ] Plan `rubyzip` upgrade (2.4.1 → 3.4.0) across three locations
   - [ ] Review API changes and test compatibility
   - [ ] Update all three Gemfile.lock files
   - [ ] Execute full regression testing
   - **Timeline:** Within 1 week
   - **Estimated Effort:** 5 story points

### Ongoing (Next Sprint)

4. **Priority 4 - Medium Vulnerabilities**
   - [ ] Update activesupport to 7.2.3.1
   - [ ] Update aws-sdk-s3 to 1.208.0
   - [ ] Update bcrypt to 3.1.22
   - [ ] Update mail to 2.9.1
   - [ ] Update loofah to 2.25.2
   - [ ] Schedule in next maintenance cycle
   - **Timeline:** Within 2 weeks

---

## Implementation Checklist

For each vulnerability fix, ensure:

- [ ] Dependency version updated in all affected Gemfile/Gemfile.lock files
- [ ] `bundle install` executed and Gemfile.lock committed
- [ ] Unit tests pass for affected component (make all, bundle exec rspec)
- [ ] Integration tests pass for affected service
- [ ] Manual smoke testing of affected functionality performed
- [ ] Code review completed
- [ ] Security rescan confirms vulnerability is resolved
- [ ] Deployment plan documented

---

## Full Scan Results

**Full JSON Results:** `dev-docs/security-scans/grype-20261021T032204Z.json` (122 KB)  
**Fixable Critical/High Results:** `dev-docs/security-scans/grype-20261021T032204Z-fixable-critical-high.json` (5.5 KB)

To view detailed information about all vulnerabilities:
```bash
jq '.matches[] | {package: .artifact.name, version: .artifact.version, severity: .vulnerability.severity, id: .vulnerability.id}' dev-docs/security-scans/grype-20261021T032204Z.json
```

---

## Next Steps

1. **Approve Remediation Plan** - Review and approve this security assessment
2. **Create Jira Stories** - Convert Priority 1 & 2 items into urgent security stories
3. **Execute Fixes** - Implement patches following the timeline above
4. **Verify Resolution** - Re-run Grype scan to confirm all vulnerabilities are resolved
5. **Monitor & Report** - Document completion and communicate security posture improvement

---

## Support Resources

- **Grype Scan Files:** `/dev-docs/security-scans/`
- **Dependency Management:** Review and update Gemfile/Gemfile.lock in each component
- **Security Advisories:** Consult GitHub Security Advisory links in vulnerability details
- **NVD CVE Details:** Full CVE information available at nvd.nist.gov

**Report Generated:** 2026-10-20T23:22:04Z  
**Database Status:** Valid (2026-10-20T06:29:49Z, Schema v6.1.10)
