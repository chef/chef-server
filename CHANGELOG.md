# Enterprise Chef Changelog

## 1.4.9 (2014-03-??)

### private-chef-cookbooks
* [OC-9213] Provide default value for jetty log path to prevent deletion of files in ``/root``.

### opscode-omnibus
* argument validation fix in private-chef-ctl to allow ``private-chef-ctl password`` to work.

## 1.4.8 (2014-02-28)

### private-chef-cookbooks
* Remove :session and :environment from webui exception emails

## 1.4.7 (2014-02-17)

### libyaml 0.1.5
* [CVE-2013-6393] - ml_parser_scan_tag_uri function in scanner.c performs incorrect cast

### openssl 1.0.1f
* [CVE-2013-4353] - allows remote TLS servers to cause a denial of service

### nginx 1.4.4
* [CVE-2013-2070] - when proxy_pass is used with untrusted HTTP servers, allows remote attackers to cause a denial of service
* [CVE-2013-4547] - allows remote attackers to bypass intended restrictions via an unescaped space character in a URI

### ruby 1.9.3-p484
* [CVE-2013-4164] - heap-based buffer overflow allows context-dependent attackers to cause a denial of service (segmentation fault) and possibly execute arbitrary code via a string that is converted to a floating point value

### postgresql 9.1.9
* [CVE-2013-1899] - allows remote attackers to cause a denial of service (file corruption), and allows remote authenticated users to modify configuration settings and execute arbitrary code
* [CVE-2013-1900] - when using OpenSSL, generates insufficiently random numbers, which might allow remote authenticated users to have an unspecified impact via vectors related to the "contrib/pgcrypto functions"
* [CVE-2013-1901] - does not properly check REPLICATION privileges, which allows remote authenticated users to bypass intended backup restrictions by calling the (1) pg_start_backup or (2) pg_stop_backup functions
* [CVE-2013-1902] - generates insecure temporary files with predictable filenames, which has unspecified impact and attack vectors related to "graphical installers for Linux and Mac OS X"
* [CVE-2013-1903] - incorrectly provides the superuser password to scripts related to "graphical installers for Linux and Mac OS X," which has unspecified impact and attack vectors
