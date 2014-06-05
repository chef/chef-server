# Enterprise Chef Changelog

## 1.4.11 (2014-06-05)

### openssl 1.0.1h
* Address vulnerabilities CVE-2014-0224, CVE-2014-0221, CVE-2014-0195,
  CVE-2014-3470 https://www.openssl.org/news/secadv_20140605.txt

## 1.4.10 (2014-05-07)

### opscode-account 1.30.7.1
* Prevent password authentication for pivotal superuser

### private-chef-cookbooks
* Set random initial password for pivotal user on bootstrap

## 1.4.9 (2014-04-09)

### curl 7.36.0
* CVE-2014-0138: libcurl can in some circumstances re-use the wrong connection when asked to do transfers using other protocols than HTTP and FTP
* CVE-2014-0139: libcurl incorrectly validates wildcard SSL certificates containing literal IP addresses when built to use OpenSSL
* CVE-2014-1263: When asked to do a TLS connection (HTTPS, FTPS, IMAPS, etc) to a URL specified with an IP address instead of a name, libcurl built to use Darwinssl would wrongly not verify the server's name in the certificate
* CVE-2014-2522: When asked to do a TLS connection (HTTPS, FTPS, IMAPS, etc) to a URL specified with an IP address instead of a name, libcurl built to use Winssl would wrongly not verify the server's name in the certificate

### libyaml 0.1.6
* CVE-2014-2525: Heap-based buffer overflow allows context-dependent attackers to execute arbitrary code

### private-chef-cookbooks
* [OC-9213] Provide default value for jetty log path to prevent deletion of files in ``/root``.

### openssl 1.0.1f
* CVE-2014-0160: heartbeat extension allows remote attackers to obtain sensitive information from process memory

### opscode-omnibus
* argument validation fix in private-chef-ctl to allow ``private-chef-ctl password`` to work.
* Manage permissions for /var/log/opscode for non 0022 umasks

### opscode-webui
* [CVE-2014-0082] - Denial of Service Vulnerability in Action View when using render :text
* [CVE-2013-6414] - Denial of Service Vulnerability in Action View
* [CVE-2013-4389] - Possible DoS Vulnerability in Action Mailer

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
