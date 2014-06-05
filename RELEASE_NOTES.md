# Enterprise Chef Release Notes

## 1.4.11 (2014-06-05)

### Security Fixes:

The following items are the set of security fixes that have been
applied since Enterprise Chef 1.4.10:

* Address vulnerabilities CVE-2014-0224, CVE-2014-0221, CVE-2014-0195,
  CVE-2014-3470 https://www.openssl.org/news/secadv_20140605.txt

## 1.4.10 (2014-05-07)

### Security Fixes:

The following items are the set of security fixes that have been
applied since Enterprise Chef 1.4.9:

* [bootstrap] Set random initial password for pivotal superuser on bootstrap
# [opscode-account] Prevent password authentication for pivotal superuser

## 1.4.9 (2014-04-09)

### Bug Fixes:

The following bug fixes have been applied since Enterprise Chef 1.4.8:

* [private-chef-cookbooks] Provide default value for jetty log path to fix dotfile deletion bug.
* [private-chef-ctl] ``private-chef-ctl password`` fixed to work correctly after a regression introduced in EC 1.4.6.
* [private-chef-cookbooks] Manage /var/log/opscode permissions even with non 0022 umask.

### Security Fixes

The following items are the set of security fixes that have been applied
since Enterprise Chef 1.4.8

* [opscode-webui] Incorporate atches to address several identified potential Denial of Service vulnerabilities in the Rails framework.
* [libcurl] Patch for wrong re-use of connections (CVE-2014-0138)
* [libcurl] Patch for address wildcard certificate validation (CVE-2014-0139)
* [libcurl] Patch for not verifying certs for TLS to IP address / Darwinssl (CVE-2014-1563)
* [libcurl] Patch for not verifying certs for TLS to IP address / Winssl (CVE-2014-2522)
* [openssl] Patch for heartbeat extension exposing process memory (CVE-2014-0160)
* [libyaml] Patch for arbitrary code execution vulnerability (CVE-2014-2525)

## 1.4.8 (2014-02-28)

### Security Fixes:

The following items are the set of security fixes that have been applied since Enterprise Chef 1.4.7:

* [opscode-webui] Don't log or email the Rails session or enviroment from the exception handler. Doing so can cause user-submitted form values like passwords to be logged and emailed to administrators of the Enterprise Chef server when exceptions occur on the Managment Console.
