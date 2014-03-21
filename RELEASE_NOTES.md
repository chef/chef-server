# Enterprise Chef Release Notes

## 11.1.3 (Unreleased)

### What's New:

The following items are new for Enterprise Chef 11.1.3 and/or are changes from previous versions:

### Bug Fixes:

The following items are the set of bug fixes that have been applied since Enterprise Chef 11.1.2:

* [opscode-omnibus] Increased postgresql max_connections to a default of 350 to handle 4 node clusters.
* [opscode-account] Fix for LDAP user creation failure.
* [opscode-omnibus] Manage /var/log/opscode permissions even with non 0022 umask.

### Security Fixes:

The following items are the set of security fixes that have been
applied since Enterprise Chef 11.1.2:

* [opscode-webui] Patch for Denial of Service Vulnerability in Action View when using render :text (CVE-2014-0082)
* [opscode-webui] Patch for Denial of Service Vulnerability in Action View (CVE-2013-6414)
* [opscode-webui] Patch for Reflective XSS Vulnerability in Ruby on Rails (CVE-2013-4491)

## 11.1.2 (2014-02-28)

### Security Fixes:

The following items are the set of security fixes that have been applied since Enterprise Chef 11.1.1:

* [opscode-webui] Don't log or email the Rails session or environment from the exception handler. Doing so can cause user-submitted form values like passwords to be logged and emailed to administrators of the Enterprise Chef server when exceptions occur on the Management Console.
