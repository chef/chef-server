# Chef Server Changelog
<!-- usage documentation: http://expeditor-docs.es.chef.io/configuration/changelog/ -->
<!-- latest_release 13.2.3 -->
## [13.2.3](https://github.com/chef/chef-server/tree/13.2.3) (2020-04-24)

#### Merged Pull Requests
- Add an exception for habitat core/perl licenses. [#1972](https://github.com/chef/chef-server/pull/1972) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
<!-- latest_release -->

<!-- release_rollup since=13.2.0 -->
### Changes since 13.2.0 release

#### Merged Pull Requests
- Add an exception for habitat core/perl licenses. [#1972](https://github.com/chef/chef-server/pull/1972) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit)) <!-- 13.2.3 -->
- Turn off smart builds [#1965](https://github.com/chef/chef-server/pull/1965) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit)) <!-- 13.2.2 -->
- Update release notes for 13.2.0 [#1964](https://github.com/chef/chef-server/pull/1964) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit)) <!-- 13.2.1 -->
<!-- release_rollup -->

<!-- latest_stable_release -->
## [13.2.0](https://github.com/chef/chef-server/tree/13.2.0) (2020-04-14)

#### Merged Pull Requests
- Add integration_test pipeline [#1857](https://github.com/chef/chef-server/pull/1857) ([christopher-snapp](https://github.com/christopher-snapp))
- Bump Chef dep to 15.5 [#1853](https://github.com/chef/chef-server/pull/1853) ([tas50](https://github.com/tas50))
- Bump rubyzip in oc-id to resolve CVE [#1854](https://github.com/chef/chef-server/pull/1854) ([tas50](https://github.com/tas50))
- Match the version of Chef Infra Client for habitat with the version in omnibus_overrides. [#1859](https://github.com/chef/chef-server/pull/1859) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Refactor external postgresql scenarios for consistency [#1858](https://github.com/chef/chef-server/pull/1858) ([christopher-snapp](https://github.com/christopher-snapp))
- Update Ruby from 2.6.3 to 2.6.5 [#1851](https://github.com/chef/chef-server/pull/1851) ([tas50](https://github.com/tas50))
- Apply cookstyle to the cookbooks [#1852](https://github.com/chef/chef-server/pull/1852) ([tas50](https://github.com/tas50))
- Update RELEASE_NOTES for 13.1.13 promotion [#1855](https://github.com/chef/chef-server/pull/1855) ([christopher-snapp](https://github.com/christopher-snapp))
- Pin perl to 5.18.1 [#1864](https://github.com/chef/chef-server/pull/1864) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- point to master branch of omnibus-software [#1861](https://github.com/chef/chef-server/pull/1861) ([lbakerchef](https://github.com/lbakerchef))
- ci: disable microsoft mirror, exit bk_install script on first error [#1865](https://github.com/chef/chef-server/pull/1865) ([stevendanna](https://github.com/stevendanna))
- Add scenario for chef-backend [#1860](https://github.com/chef/chef-server/pull/1860) ([christopher-snapp](https://github.com/christopher-snapp))
- Fix chef-backend scenario to work on ubuntu [#1868](https://github.com/chef/chef-server/pull/1868) ([christopher-snapp](https://github.com/christopher-snapp))
- Fix two small chef-server-ctl typos [#1875](https://github.com/chef/chef-server/pull/1875) ([ehershey](https://github.com/ehershey))
- Bump rack from 1.6.11 to 1.6.12 in /src/oc-id [#1873](https://github.com/chef/chef-server/pull/1873) ([dependabot[bot]](https://github.com/dependabot[bot]))
- Bump rack from 2.0.7 to 2.0.8 in /oc-chef-pedant [#1872](https://github.com/chef/chef-server/pull/1872) ([dependabot[bot]](https://github.com/dependabot[bot]))
- Add buildkite steps to the `integration_test` pipeline [#1874](https://github.com/chef/chef-server/pull/1874) ([christopher-snapp](https://github.com/christopher-snapp))
- Minor integration_test pipeline tweaks [#1883](https://github.com/chef/chef-server/pull/1883) ([christopher-snapp](https://github.com/christopher-snapp))
- More fixes to integration_test script [#1889](https://github.com/chef/chef-server/pull/1889) ([christopher-snapp](https://github.com/christopher-snapp))
- Add fail-over test to chef-backend scenario [#1885](https://github.com/chef/chef-server/pull/1885) ([christopher-snapp](https://github.com/christopher-snapp))
- Add external elasticsearch terraform scenario [#1894](https://github.com/chef/chef-server/pull/1894) ([christopher-snapp](https://github.com/christopher-snapp))
- fix erlang dependency for habitat build [#1902](https://github.com/chef/chef-server/pull/1902) ([uwej711](https://github.com/uwej711))
- fix habitat db config for external database [#1903](https://github.com/chef/chef-server/pull/1903) ([uwej711](https://github.com/uwej711))
- Point to Chef-owned Hoax repo [#1892](https://github.com/chef/chef-server/pull/1892) ([lbakerchef](https://github.com/lbakerchef))
- Adding scenario for restoring from a local backup [#1891](https://github.com/chef/chef-server/pull/1891) ([tyler-ball](https://github.com/tyler-ball))
- Remove encouragement to install deprecated Manage plugin [#1906](https://github.com/chef/chef-server/pull/1906) ([moutons](https://github.com/moutons))
- Replace self-signed certs in external postgresql scenario [#1907](https://github.com/chef/chef-server/pull/1907) ([christopher-snapp](https://github.com/christopher-snapp))
- Add FIPS scenario to integration tests [#1909](https://github.com/chef/chef-server/pull/1909) ([christopher-snapp](https://github.com/christopher-snapp))
- Fix integration test check for FIPS being enabled [#1912](https://github.com/chef/chef-server/pull/1912) ([christopher-snapp](https://github.com/christopher-snapp))
- Add license exception for nss-myhostname [#1913](https://github.com/chef/chef-server/pull/1913) ([christopher-snapp](https://github.com/christopher-snapp))
- Lbaker/zanecodes connection user [#1728](https://github.com/chef/chef-server/pull/1728) ([lbakerchef](https://github.com/lbakerchef))
- Fixes and improvements to the integration_test pipeline [#1917](https://github.com/chef/chef-server/pull/1917) ([christopher-snapp](https://github.com/christopher-snapp))
- Update pointers to erlware_commons [#1919](https://github.com/chef/chef-server/pull/1919) ([lbakerchef](https://github.com/lbakerchef))
- Allow for disabling specific tests in scenarios [#1918](https://github.com/chef/chef-server/pull/1918) ([christopher-snapp](https://github.com/christopher-snapp))
- Update scenarios to default to t3.medium [#1920](https://github.com/chef/chef-server/pull/1920) ([christopher-snapp](https://github.com/christopher-snapp))
- Add architecture to AMI detection to prevent false matches [#1923](https://github.com/chef/chef-server/pull/1923) ([christopher-snapp](https://github.com/christopher-snapp))
- Update Chef Infra Client from 15.5.17 to 15.8.23 [#1921](https://github.com/chef/chef-server/pull/1921) ([tas50](https://github.com/tas50))
- Update jre path for PATH and JAVA_HOME variables [#1925](https://github.com/chef/chef-server/pull/1925) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Update HAProxy configuration [#1785](https://github.com/chef/chef-server/pull/1785) ([stevendanna](https://github.com/stevendanna))
- Point elasticsearch to OpenJDK [#1927](https://github.com/chef/chef-server/pull/1927) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Fix Elasticsearch scenario failing to consistently install + start elasticsearch [#1922](https://github.com/chef/chef-server/pull/1922) ([christopher-snapp](https://github.com/christopher-snapp))
- Update integration tests [#1928](https://github.com/chef/chef-server/pull/1928) ([christopher-snapp](https://github.com/christopher-snapp))
- Update deprecated Net::HTTPServerException with Net::HttpClientException [#1930](https://github.com/chef/chef-server/pull/1930) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Assume modern Linux in our haproxy omnibus builds [#1764](https://github.com/chef/chef-server/pull/1764) ([tas50](https://github.com/tas50))
- Allow ELASTIC_VERSION to be passed in to integration_test pipeline [#1933](https://github.com/chef/chef-server/pull/1933) ([christopher-snapp](https://github.com/christopher-snapp))
- Disable actions since it was used only for oc_actions, analytics and workflow. [#1929](https://github.com/chef/chef-server/pull/1929) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Document manual cleanup of terraform resources [#1937](https://github.com/chef/chef-server/pull/1937) ([christopher-snapp](https://github.com/christopher-snapp))
- Update terraform from 0.11 to latest 0.12 [#1938](https://github.com/chef/chef-server/pull/1938) ([christopher-snapp](https://github.com/christopher-snapp))
- We do not support the addons push-jobs and manage in FIPS mode. [#1939](https://github.com/chef/chef-server/pull/1939) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Allow version 6 of elasticsearch along with 2 and 5. [#1940](https://github.com/chef/chef-server/pull/1940) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Fix integration test cancellation [#1942](https://github.com/chef/chef-server/pull/1942) ([christopher-snapp](https://github.com/christopher-snapp))
- Fix integration test security group prefix and integration test cancellation [#1944](https://github.com/chef/chef-server/pull/1944) ([christopher-snapp](https://github.com/christopher-snapp))
- For CPUs with the AES New Instructions add dracut-fips-aesni to turn … [#1943](https://github.com/chef/chef-server/pull/1943) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Fix pedant to not be strict about cookbook artifact metadata [#1948](https://github.com/chef/chef-server/pull/1948) ([lamont-granquist](https://github.com/lamont-granquist))
- Praj/enable internal es [#1946](https://github.com/chef/chef-server/pull/1946) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Praj/fips [#1941](https://github.com/chef/chef-server/pull/1941) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Do not create indexes for elasticsearch at compile time [#1961](https://github.com/chef/chef-server/pull/1961) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Praj/1901 [#1945](https://github.com/chef/chef-server/pull/1945) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Update version to 13.2 [#1963](https://github.com/chef/chef-server/pull/1963) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
<!-- latest_stable_release -->

## [13.1.13](https://github.com/chef/chef-server/tree/13.1.13) (2019-11-25)

#### Merged Pull Requests
- Update yard to current [#1696](https://github.com/chef/chef-server/pull/1696) ([tas50](https://github.com/tas50))
- Update github templates, add lock config, and update codeowners [#1695](https://github.com/chef/chef-server/pull/1695) ([tas50](https://github.com/tas50))
- Bump bundler to 1.17.3 [#1705](https://github.com/chef/chef-server/pull/1705) ([markan](https://github.com/markan))
- Dvm changes to get external postgres working. [#1708](https://github.com/chef/chef-server/pull/1708) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Remove the old delivery directory [#1694](https://github.com/chef/chef-server/pull/1694) ([tas50](https://github.com/tas50))
- Fix logins to zendesk when user signups with a single name [#1710](https://github.com/chef/chef-server/pull/1710) ([teknofire](https://github.com/teknofire))
- implement 1703 exclude data collector from status endpoint [#1716](https://github.com/chef/chef-server/pull/1716) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Doc updates [#1699](https://github.com/chef/chef-server/pull/1699) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Update the release process doc with current world process. [#1700](https://github.com/chef/chef-server/pull/1700) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Add buildkite specific checks to pull request template. [#1715](https://github.com/chef/chef-server/pull/1715) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Bump postgresql from 9.6.10 to 9.6.14 (latest minor) [#1717](https://github.com/chef/chef-server/pull/1717) ([btm](https://github.com/btm))
- Stop building Chef Infra Server on SLES 11 [#1718](https://github.com/chef/chef-server/pull/1718) ([schisamo](https://github.com/schisamo))
- fix X-Forwared-For typo [#1719](https://github.com/chef/chef-server/pull/1719) ([srenatus](https://github.com/srenatus))
- Zanecodes ssl [#1711](https://github.com/chef/chef-server/pull/1711) ([lbakerchef](https://github.com/lbakerchef))
- Praj/add elasticsearch support [#1721](https://github.com/chef/chef-server/pull/1721) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Update to the latest non-license version of chef. [#1722](https://github.com/chef/chef-server/pull/1722) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Update README.md [#1723](https://github.com/chef/chef-server/pull/1723) ([lbakerchef](https://github.com/lbakerchef))
- Update the omnibus kitchen.yml to the latest non-licensed version of chef [#1726](https://github.com/chef/chef-server/pull/1726) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Praj/remove deprecations [#1727](https://github.com/chef/chef-server/pull/1727) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Enable RHEL 8 packages [#1707](https://github.com/chef/chef-server/pull/1707) ([jaymalasinha](https://github.com/jaymalasinha))
- development environment improvements, mostly for oc-id [#1724](https://github.com/chef/chef-server/pull/1724) ([robbkidd](https://github.com/robbkidd))
- Documentation to include updating to rebar3 and updating with rebar3 [#1746](https://github.com/chef/chef-server/pull/1746) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Update Feature Request template to point to the Aha! Portal. [#1730](https://github.com/chef/chef-server/pull/1730) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Update tests to use ruby 2.5.6 [#1756](https://github.com/chef/chef-server/pull/1756) ([christopher-snapp](https://github.com/christopher-snapp))
- Bump omnibus chef-infra to 15.4.20 [#1774](https://github.com/chef/chef-server/pull/1774) ([christopher-snapp](https://github.com/christopher-snapp))
- Cookstyle updates for the chef-server-deploy cookbook [#1762](https://github.com/chef/chef-server/pull/1762) ([tas50](https://github.com/tas50))
- Remove foodcritic testing from Buildkite [#1763](https://github.com/chef/chef-server/pull/1763) ([tas50](https://github.com/tas50))
- Initial refactor of Terraform for on-demand integration testing [#1768](https://github.com/chef/chef-server/pull/1768) ([christopher-snapp](https://github.com/christopher-snapp))
- Update the command to do the right thing [#1747](https://github.com/chef/chef-server/pull/1747) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- update openresty [#1706](https://github.com/chef/chef-server/pull/1706) ([markan](https://github.com/markan))
- Add support for testing a scenario via IPv4 or IPv6 [#1788](https://github.com/chef/chef-server/pull/1788) ([christopher-snapp](https://github.com/christopher-snapp))
- Bump nokogiri from 1.8.5 to 1.10.4 in /src/oc-id [#1765](https://github.com/chef/chef-server/pull/1765) ([dependabot[bot]](https://github.com/dependabot[bot]))
- [chef-server-ctl] Install only appbundled version [#1533](https://github.com/chef/chef-server/pull/1533) ([markan](https://github.com/markan))
- removing forcing of the doc formatter [#1469](https://github.com/chef/chef-server/pull/1469) ([lamont-granquist](https://github.com/lamont-granquist))
- Add copy/paste execution strings for convenient builds [#1750](https://github.com/chef/chef-server/pull/1750) ([lbakerchef](https://github.com/lbakerchef))
- Knife-tidy was newly added as a dependency to knife-ec-backup. [#1792](https://github.com/chef/chef-server/pull/1792) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- upgrade rebar3 to 3.6.2 [#1748](https://github.com/chef/chef-server/pull/1748) ([lbakerchef](https://github.com/lbakerchef))
- Expand terraform to work across the matrix of distributions [#1791](https://github.com/chef/chef-server/pull/1791) ([christopher-snapp](https://github.com/christopher-snapp))
- Revert the only change between versions failing tests with command not found on vagrant. [#1793](https://github.com/chef/chef-server/pull/1793) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Update pinned version of chef-infra to 15.4.45 [#1794](https://github.com/chef/chef-server/pull/1794) ([christopher-snapp](https://github.com/christopher-snapp))
- Disable auto-ipv6 allocation if ENABLE_IPV6=false [#1795](https://github.com/chef/chef-server/pull/1795) ([christopher-snapp](https://github.com/christopher-snapp))
- Lbaker/land phase2 [#1789](https://github.com/chef/chef-server/pull/1789) ([lbakerchef](https://github.com/lbakerchef))
- Lbaker/land erlang19 [#1790](https://github.com/chef/chef-server/pull/1790) ([lbakerchef](https://github.com/lbakerchef))
- Resolve cookstyle warnings in the cookbooks and remove an old .rubocop.yml file [#1807](https://github.com/chef/chef-server/pull/1807) ([tas50](https://github.com/tas50))
- Modify dev/Vagrantfile to handle chef licensing [#1811](https://github.com/chef/chef-server/pull/1811) ([christopher-snapp](https://github.com/christopher-snapp))
- Split up terraform scenarios&#39; remote-exec scripts [#1816](https://github.com/chef/chef-server/pull/1816) ([christopher-snapp](https://github.com/christopher-snapp))
- Add external openldap terraform scenario [#1812](https://github.com/chef/chef-server/pull/1812) ([christopher-snapp](https://github.com/christopher-snapp))
- fix erlang build version, dialyzer warnings, compile warnings [#1817](https://github.com/chef/chef-server/pull/1817) ([lbakerchef](https://github.com/lbakerchef))
- Add testing of Push Jobs add-on [#1823](https://github.com/chef/chef-server/pull/1823) ([christopher-snapp](https://github.com/christopher-snapp))
- Add testing of Chef Manage add-on [#1824](https://github.com/chef/chef-server/pull/1824) ([christopher-snapp](https://github.com/christopher-snapp))
- Bump loofah from 2.2.3 to 2.3.1 in /src/oc-id [#1820](https://github.com/chef/chef-server/pull/1820) ([dependabot[bot]](https://github.com/dependabot[bot]))
- Lbaker/land erlang20 final [#1813](https://github.com/chef/chef-server/pull/1813) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Upgrading to postgres 9.6.15 [#1828](https://github.com/chef/chef-server/pull/1828) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- restructure terraform to make way for azure scenarios [#1829](https://github.com/chef/chef-server/pull/1829) ([christopher-snapp](https://github.com/christopher-snapp))
- updates to chef-server-ctl gather-logs [#1814](https://github.com/chef/chef-server/pull/1814) ([moutons](https://github.com/moutons))
- Add Terraform scenario for Azure PostgreSQL [#1826](https://github.com/chef/chef-server/pull/1826) ([christopher-snapp](https://github.com/christopher-snapp))
- Add ruby 2.5.6 to verify pipeline [#1831](https://github.com/chef/chef-server/pull/1831) ([christopher-snapp](https://github.com/christopher-snapp))
- Add a scenario to test chef-server tiered upgrade  [#1830](https://github.com/chef/chef-server/pull/1830) ([christopher-snapp](https://github.com/christopher-snapp))
- Bump Chef Infra Server version to 13.1.0 [#1832](https://github.com/chef/chef-server/pull/1832) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Add chef-server-ctl psql tests to terraform [#1834](https://github.com/chef/chef-server/pull/1834) ([christopher-snapp](https://github.com/christopher-snapp))
- Speed up Terraform Push Jobs tests [#1836](https://github.com/chef/chef-server/pull/1836) ([christopher-snapp](https://github.com/christopher-snapp))
- Fix 502 gateway timeout errors in terraform [#1837](https://github.com/chef/chef-server/pull/1837) ([christopher-snapp](https://github.com/christopher-snapp))
- Add standalone fresh install terraform scenario [#1838](https://github.com/chef/chef-server/pull/1838) ([christopher-snapp](https://github.com/christopher-snapp))
- The byos ami for sles does not allow updates. Using a pay as you go version. [#1835](https://github.com/chef/chef-server/pull/1835) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Fix terraform tiered pedant tests [#1840](https://github.com/chef/chef-server/pull/1840) ([christopher-snapp](https://github.com/christopher-snapp))
- Do not overwrite :body and :response_body [#1841](https://github.com/chef/chef-server/pull/1841) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Use localhost instead of the public IP address for ipv6 [#1842](https://github.com/chef/chef-server/pull/1842) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Update the ldap test fixtures [#1844](https://github.com/chef/chef-server/pull/1844) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Remove Ruby 2.5.6 Pinning [#1846](https://github.com/chef/chef-server/pull/1846) ([christopher-snapp](https://github.com/christopher-snapp))
- Adding Terraform scenario for external-postgres [#1845](https://github.com/chef/chef-server/pull/1845) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Enable LDAP Testing in pedant_config [#1847](https://github.com/chef/chef-server/pull/1847) ([christopher-snapp](https://github.com/christopher-snapp))
- Fix terraform rhel-6 tiered-upgrade compliance tests [#1849](https://github.com/chef/chef-server/pull/1849) ([christopher-snapp](https://github.com/christopher-snapp))

## [13.0.17](https://github.com/chef/chef-server/tree/13.0.17) (2019-07-01)

#### Merged Pull Requests
- Remove 12.19.* version constraint on master [#1693](https://github.com/chef/chef-server/pull/1693) ([markan](https://github.com/markan))

## [13.0.16](https://github.com/chef/chef-server/tree/13.0.16) (2019-07-01)

#### Merged Pull Requests
- Update gitignore [#1645](https://github.com/chef/chef-server/pull/1645) ([markan](https://github.com/markan))
- Jsinha/migrate all tests tobk [#1640](https://github.com/chef/chef-server/pull/1640) ([jaymalasinha](https://github.com/jaymalasinha))
- Fix unit tests [#1644](https://github.com/chef/chef-server/pull/1644) ([markan](https://github.com/markan))
- Parameterize the search_server in the habitized pedant config [#1629](https://github.com/chef/chef-server/pull/1629) ([irvingpop](https://github.com/irvingpop))
- Update openresty to 1.13.6.2 [#1623](https://github.com/chef/chef-server/pull/1623) ([markan](https://github.com/markan))
- Update ruby and chef versions [#1647](https://github.com/chef/chef-server/pull/1647) ([jaymalasinha](https://github.com/jaymalasinha))
- Update license scout ruby pin [#1655](https://github.com/chef/chef-server/pull/1655) ([jaymalasinha](https://github.com/jaymalasinha))
- Double the erchef max_request_size [#1649](https://github.com/chef/chef-server/pull/1649) ([irvingpop](https://github.com/irvingpop))
- Remove `/habitat` from end of `plan_path` [#1659](https://github.com/chef/chef-server/pull/1659) ([tduffield](https://github.com/tduffield))
- Update pedant to match new maximum request size [#1661](https://github.com/chef/chef-server/pull/1661) ([markan](https://github.com/markan))
- Update chef-client in one more location. [#1660](https://github.com/chef/chef-server/pull/1660) ([markan](https://github.com/markan))
- [nginx] Don&#39;t log 404s to the error log [#1663](https://github.com/chef/chef-server/pull/1663) ([stevendanna](https://github.com/stevendanna))
- render profiles and data-collector upstreams correctly [#1665](https://github.com/chef/chef-server/pull/1665) ([sdelano](https://github.com/sdelano))
- Update the omnibus build license to the Chef EULA [#1666](https://github.com/chef/chef-server/pull/1666) ([btm](https://github.com/btm))
- Ma/use specific path [#1671](https://github.com/chef/chef-server/pull/1671) ([markan](https://github.com/markan))
- Bump version to v13 for Chef EULA major release [#1667](https://github.com/chef/chef-server/pull/1667) ([btm](https://github.com/btm))
- Remove Keepalived/DRBD based HA [#1664](https://github.com/chef/chef-server/pull/1664) ([markan](https://github.com/markan))
- Remove more keepalived stuff [#1673](https://github.com/chef/chef-server/pull/1673) ([markan](https://github.com/markan))
- Ma/update runit [#1668](https://github.com/chef/chef-server/pull/1668) ([markan](https://github.com/markan))
- Remove Ubuntu 14.04 from build matrix [#1662](https://github.com/chef/chef-server/pull/1662) ([markan](https://github.com/markan))
- Add OSS Practices [#1675](https://github.com/chef/chef-server/pull/1675) ([markan](https://github.com/markan))
- Ma/named dev vm [#1670](https://github.com/chef/chef-server/pull/1670) ([markan](https://github.com/markan))
- Update observer-cli SHA. [#1677](https://github.com/chef/chef-server/pull/1677) ([markan](https://github.com/markan))
- [dev-vm] Fixup incomplete rename work. [#1678](https://github.com/chef/chef-server/pull/1678) ([markan](https://github.com/markan))
- Ma/license check [#1674](https://github.com/chef/chef-server/pull/1674) ([markan](https://github.com/markan))
- Praj/rubygems update [#1681](https://github.com/chef/chef-server/pull/1681) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Update Release Notes for release 13.0.11 [#1682](https://github.com/chef/chef-server/pull/1682) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Delete the broken link for chef-ha-drbd plugin. [#1686](https://github.com/chef/chef-server/pull/1686) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- [chef-server-ctl] Fix chef dependency in hab build [#1687](https://github.com/chef/chef-server/pull/1687) ([markan](https://github.com/markan))
- Alter the delivery build recipe to accept the chef license. [#1688](https://github.com/chef/chef-server/pull/1688) ([markan](https://github.com/markan))
- Do not run /_stats endpoint tests on backend of a tiered topology. [#1689](https://github.com/chef/chef-server/pull/1689) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- [chef-server-ctl] Fix rabbitmq fail on first reconfigure [#1691](https://github.com/chef/chef-server/pull/1691) ([markan](https://github.com/markan))

## [12.19.31](https://github.com/chef/chef-server/tree/12.19.31) (2019-03-07)

#### Merged Pull Requests
- Empty commit to rebuild hartifacts [#1625](https://github.com/chef/chef-server/pull/1625) ([ryancragun](https://github.com/ryancragun))
- This is an empty commit to test omnibus buildkite release pipeline [#1627](https://github.com/chef/chef-server/pull/1627) ([jeremiahsnapp](https://github.com/jeremiahsnapp))
- chef-server-ctl should leverage HAB_LISTEN_CTL envvar if available [#1628](https://github.com/chef/chef-server/pull/1628) ([jeremymv2](https://github.com/jeremymv2))
- Rebuild the hartifacts (OpenSSL was updated) [#1636](https://github.com/chef/chef-server/pull/1636) ([jaym](https://github.com/jaym))
- Force a habitat rebuild to pull in new openssl [#1637](https://github.com/chef/chef-server/pull/1637) ([stevendanna](https://github.com/stevendanna))

## [12.19.26](https://github.com/chef/chef-server/tree/12.19.26) (2019-01-31)

#### Merged Pull Requests
- increase authn:keygen_timeout for oc_erchef hab pkg [#1579](https://github.com/chef/chef-server/pull/1579) ([jeremymv2](https://github.com/jeremymv2))
- Merge mTLS changes. [#1572](https://github.com/chef/chef-server/pull/1572) ([markan](https://github.com/markan))
- Update chef-client to 14.4.56 [#1567](https://github.com/chef/chef-server/pull/1567) ([tas50](https://github.com/tas50))
- [oc-chef-pedant] Add logging for timeouts. [#1555](https://github.com/chef/chef-server/pull/1555) ([markan](https://github.com/markan))
- [nginx] Add request id to nginx log [#1565](https://github.com/chef/chef-server/pull/1565) ([markan](https://github.com/markan))
- [expeditor] Modernize to use subscriptions [#1568](https://github.com/chef/chef-server/pull/1568) ([markan](https://github.com/markan))
- Bump version to 12.19.0 [#1558](https://github.com/chef/chef-server/pull/1558) ([markan](https://github.com/markan))
- Use our standard ruby-cleanup definition [#1582](https://github.com/chef/chef-server/pull/1582) ([tas50](https://github.com/tas50))
- Update Chef 14.4 -&gt; 14.5 [#1583](https://github.com/chef/chef-server/pull/1583) ([tas50](https://github.com/tas50))
- Update to Ruby 2.5.3 [#1584](https://github.com/chef/chef-server/pull/1584) ([tas50](https://github.com/tas50))
- Modernize Expeditor config; Promote harts/containers [#1542](https://github.com/chef/chef-server/pull/1542) ([schisamo](https://github.com/schisamo))
- Update erlang to 18.3.4.9 [#1585](https://github.com/chef/chef-server/pull/1585) ([markan](https://github.com/markan))
- Update issue template [#1588](https://github.com/chef/chef-server/pull/1588) ([markan](https://github.com/markan))
- [chef-server-ctl] Make chef-server-ctl configurable [#1573](https://github.com/chef/chef-server/pull/1573) ([markan](https://github.com/markan))
- Server admin mutual TLS support [#1591](https://github.com/chef/chef-server/pull/1591) ([stevendanna](https://github.com/stevendanna))
- Remove a few more couchdb configurables [#1526](https://github.com/chef/chef-server/pull/1526) ([stevendanna](https://github.com/stevendanna))
- Add SSL configuration parameters to pedant [#1590](https://github.com/chef/chef-server/pull/1590) ([stevendanna](https://github.com/stevendanna))
- Add note about out of date dep to trigger a rebuild [#1601](https://github.com/chef/chef-server/pull/1601) ([btm](https://github.com/btm))
- [chef-server] Remove do_end function from chef-server-ctl hab plan [#1605](https://github.com/chef/chef-server/pull/1605) ([stevendanna](https://github.com/stevendanna))
- Pass ssl values to RestClient::Request.execute [#1608](https://github.com/chef/chef-server/pull/1608) ([btm](https://github.com/btm))
- Use the embedded env when running escript [#1589](https://github.com/chef/chef-server/pull/1589) ([stevendanna](https://github.com/stevendanna))
- [expeditor] Build chef-server-ctl when pedant changes [#1609](https://github.com/chef/chef-server/pull/1609) ([stevendanna](https://github.com/stevendanna))
- Lock bundler to 1.17 to defer bundler 2.0 upgrade [#1614](https://github.com/chef/chef-server/pull/1614) ([markan](https://github.com/markan))
- Minor cleanup the omnibus setup [#1595](https://github.com/chef/chef-server/pull/1595) ([tas50](https://github.com/tas50))
- Empty commit to force hab package builds. [#1615](https://github.com/chef/chef-server/pull/1615) ([rhass](https://github.com/rhass))
- Add license_scout overrides and exceptions for erlang libraries [#1616](https://github.com/chef/chef-server/pull/1616) ([tduffield](https://github.com/tduffield))
- Force a new build. [#1619](https://github.com/chef/chef-server/pull/1619) ([rhass](https://github.com/rhass))

## [12.18.14](https://github.com/chef/chef-server/tree/12.18.14) (2018-10-15)

#### Merged Pull Requests
- Update release notes for 12.17.33 release. [#1476](https://github.com/chef/chef-server/pull/1476) ([rhass](https://github.com/rhass))
- habitat mode for chef-server-ctl command [#1475](https://github.com/chef/chef-server/pull/1475) ([jeremymv2](https://github.com/jeremymv2))
- Adding Habitization - this PR supersedes PR#1416 [#1472](https://github.com/chef/chef-server/pull/1472) ([jeremymv2](https://github.com/jeremymv2))
- Add queue insertion information to statistics. [#1462](https://github.com/chef/chef-server/pull/1462) ([markan](https://github.com/markan))
- resolve lingering issues from PR#1472 [#1477](https://github.com/chef/chef-server/pull/1477) ([jeremymv2](https://github.com/jeremymv2))
- [upgrades] Don&#39;t early return in upgrade 1.31 [#1478](https://github.com/chef/chef-server/pull/1478) ([stevendanna](https://github.com/stevendanna))
- Ma/update all the things [#1482](https://github.com/chef/chef-server/pull/1482) ([markan](https://github.com/markan))
- lock to chef 13.x for habitat pkgs [#1483](https://github.com/chef/chef-server/pull/1483) ([jeremymv2](https://github.com/jeremymv2))
- Added chef-server-ctl version functionality [#1485](https://github.com/chef/chef-server/pull/1485) ([thomascate](https://github.com/thomascate))
- Add sysvinitrc tag [#1488](https://github.com/chef/chef-server/pull/1488) ([markan](https://github.com/markan))
- fix builds in privilege restricted environments like builder [#1489](https://github.com/chef/chef-server/pull/1489) ([jeremymv2](https://github.com/jeremymv2))
- [oc-id] Commit assets to git and remove nodejs [#1487](https://github.com/chef/chef-server/pull/1487) ([markan](https://github.com/markan))
- allow mixlib-auth 2.0 in pedant [#1491](https://github.com/chef/chef-server/pull/1491) ([lamont-granquist](https://github.com/lamont-granquist))
- Pin chef-zero to 14.0.5 in travis [#1492](https://github.com/chef/chef-server/pull/1492) ([markan](https://github.com/markan))
- Integrate expeditor hab+docker export functionality [#1493](https://github.com/chef/chef-server/pull/1493) ([irvingpop](https://github.com/irvingpop))
- adding the compliance upstream to chef-server-nginx hab pkg [#1496](https://github.com/chef/chef-server/pull/1496) ([jeremymv2](https://github.com/jeremymv2))
- Expeditor cleanups after review with releng, plus an nginx logging bugfix for dockerized environments [#1497](https://github.com/chef/chef-server/pull/1497) ([irvingpop](https://github.com/irvingpop))
- Add DateStamp to the SOLR GC log [#1498](https://github.com/chef/chef-server/pull/1498) ([markan](https://github.com/markan))
- [SUSTAIN-937] Adding resource_name to es index resource [#1500](https://github.com/chef/chef-server/pull/1500) ([nsdavidson](https://github.com/nsdavidson))
- Unlock and bump chef-mover jiffy. [#1502](https://github.com/chef/chef-server/pull/1502) ([markan](https://github.com/markan))
- Ma/update pc and rebar [#1495](https://github.com/chef/chef-server/pull/1495) ([markan](https://github.com/markan))
- Pull in new enterprise chef common [#1501](https://github.com/chef/chef-server/pull/1501) ([markan](https://github.com/markan))
- Fix enterprise chef common typo [#1507](https://github.com/chef/chef-server/pull/1507) ([markan](https://github.com/markan))
- Include the fixie utility in chef-server [#1508](https://github.com/chef/chef-server/pull/1508) ([markan](https://github.com/markan))
- Allow find in gather-logs to follow symlinks. [#1509](https://github.com/chef/chef-server/pull/1509) ([teknofire](https://github.com/teknofire))
- Upgrade to Chef 14 [#1506](https://github.com/chef/chef-server/pull/1506) ([markan](https://github.com/markan))
- Segment-free cookbook data (RFC 67) [#1034](https://github.com/chef/chef-server/pull/1034) ([thommay](https://github.com/thommay))
- Update os versions in omnibus kitchen.yml [#1505](https://github.com/chef/chef-server/pull/1505) ([markan](https://github.com/markan))
- MSYS-848 : Prevent Chef Server headers from exposing openresty name and version [#1514](https://github.com/chef/chef-server/pull/1514) ([piyushawasthi](https://github.com/piyushawasthi))
- Ma/appbundle and move [#1516](https://github.com/chef/chef-server/pull/1516) ([markan](https://github.com/markan))
- Update oc_id gems [#1518](https://github.com/chef/chef-server/pull/1518) ([markan](https://github.com/markan))
- Ma/timestamps in utc [#1503](https://github.com/chef/chef-server/pull/1503) ([markan](https://github.com/markan))
- support HEAD http request on named node endpoint [#1218](https://github.com/chef/chef-server/pull/1218) ([jeremymv2](https://github.com/jeremymv2))
- Update Gemfiles and other version locks [#1520](https://github.com/chef/chef-server/pull/1520) ([markan](https://github.com/markan))
- Resolving Foodcritic warnings &amp; cleaning up licensing [#1510](https://github.com/chef/chef-server/pull/1510) ([tas50](https://github.com/tas50))
- Add Nolan and Prajakta as maintainers [#1521](https://github.com/chef/chef-server/pull/1521) ([markan](https://github.com/markan))
- Update Ruby to 2.5.1 [#1522](https://github.com/chef/chef-server/pull/1522) ([rhass](https://github.com/rhass))
- Update doorkeeper gem to 4.4.0 [#1523](https://github.com/chef/chef-server/pull/1523) ([rhass](https://github.com/rhass))
- Update custom resource in chef-server-deploy. [#1525](https://github.com/chef/chef-server/pull/1525) ([rhass](https://github.com/rhass))
- Update the Release Process document [#1409](https://github.com/chef/chef-server/pull/1409) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Trigger an RC build. [#1527](https://github.com/chef/chef-server/pull/1527) ([rhass](https://github.com/rhass))
- Prepare 12.18 Chef Server release. [#1528](https://github.com/chef/chef-server/pull/1528) ([rhass](https://github.com/rhass))
- Fix habitat build for the chef-server-ctl container [#1524](https://github.com/chef/chef-server/pull/1524) ([irvingpop](https://github.com/irvingpop))
- Update sqerl [#1537](https://github.com/chef/chef-server/pull/1537) ([jaym](https://github.com/jaym))
- [oc-id] Update Gemfile to pick up new rubyzip [#1534](https://github.com/chef/chef-server/pull/1534) ([markan](https://github.com/markan))
- Allow SSL configuration in bifrost sys.config [#1541](https://github.com/chef/chef-server/pull/1541) ([danielsdeleo](https://github.com/danielsdeleo))
- Postgres listens on loopbacks, not localhost [#1540](https://github.com/chef/chef-server/pull/1540) ([markan](https://github.com/markan))
- Remove old github changelog generator [#1530](https://github.com/chef/chef-server/pull/1530) ([tas50](https://github.com/tas50))
- Update tag for enterprise cookbook. [#1551](https://github.com/chef/chef-server/pull/1551) ([rhass](https://github.com/rhass))
- Update postgresql [#1539](https://github.com/chef/chef-server/pull/1539) ([markan](https://github.com/markan))
- Update oc-bifrost-pedant to use rest-client 1.8.X [#1547](https://github.com/chef/chef-server/pull/1547) ([tas50](https://github.com/tas50))
- Add config for 12.18 release branch. [#1557](https://github.com/chef/chef-server/pull/1557) ([markan](https://github.com/markan))

## [12.17.33](https://github.com/chef/chef-server/tree/12.17.33) (2018-02-23)

#### Merged Pull Requests
- Update RELEASE_NOTES.md for 12.17.15 release [#1441](https://github.com/chef/chef-server/pull/1441) ([btm](https://github.com/btm))
- It is 2017! Postpone expiration to 2025 [#1443](https://github.com/chef/chef-server/pull/1443) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Praj/sustain 633 [#1438](https://github.com/chef/chef-server/pull/1438) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Converge runners with --no-fork [#1445](https://github.com/chef/chef-server/pull/1445) ([scotthain](https://github.com/scotthain))
- [SUSTAIN-807] Clean up oc_id PID on restart [#1442](https://github.com/chef/chef-server/pull/1442) ([nsdavidson](https://github.com/nsdavidson))
- Update Chef Client and Ruby version. [#1448](https://github.com/chef/chef-server/pull/1448) ([rhass](https://github.com/rhass))
- [MSYS-722] Escape forward slash for elasticsearch query only. [#1444](https://github.com/chef/chef-server/pull/1444) ([piyushawasthi](https://github.com/piyushawasthi))
- Update release process documentation. [#1452](https://github.com/chef/chef-server/pull/1452) ([rhass](https://github.com/rhass))
- Add yml header to expeditor config. [#1453](https://github.com/chef/chef-server/pull/1453) ([rhass](https://github.com/rhass))
- omnibus: pin openresty 1.11.2.1 [#1457](https://github.com/chef/chef-server/pull/1457) ([srenatus](https://github.com/srenatus))
- Force a release build. [#1458](https://github.com/chef/chef-server/pull/1458) ([rhass](https://github.com/rhass))
- Force a release build. [#1459](https://github.com/chef/chef-server/pull/1459) ([rhass](https://github.com/rhass))
- Bump verison for Release. [#1461](https://github.com/chef/chef-server/pull/1461) ([rhass](https://github.com/rhass))
- Add observer_cli to erlang deps [#1460](https://github.com/chef/chef-server/pull/1460) ([markan](https://github.com/markan))
- Flip some bits for tests on PPC64. [#1464](https://github.com/chef/chef-server/pull/1464) ([rhass](https://github.com/rhass))
- Revert &quot;Flip some bits for tests on PPC64.&quot; [#1466](https://github.com/chef/chef-server/pull/1466) ([rhass](https://github.com/rhass))
- Use the version of LicenseScout that comes with the Omnibus gem. [#1468](https://github.com/chef/chef-server/pull/1468) ([tduffield](https://github.com/tduffield))
- Increase Net::HTTP / rest-client open_timeout [#1470](https://github.com/chef/chef-server/pull/1470) ([rhass](https://github.com/rhass))

## [12.17.15](https://github.com/chef/chef-server/tree/12.17.15) (2017-12-21)

#### Merged Pull Requests
- [SUSTAIN-728] Add /_acl for cookbook_artifacts [#1414](https://github.com/chef/chef-server/pull/1414) ([nsdavidson](https://github.com/nsdavidson))
- [rabbitmq] Remove guest user by default [#1420](https://github.com/chef/chef-server/pull/1420) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- InSpec smoke test improvements [#1423](https://github.com/chef/chef-server/pull/1423) ([schisamo](https://github.com/schisamo))
- Pull in new knife-opc (we track master) for SUSTAIN-784 [#1426](https://github.com/chef/chef-server/pull/1426) ([btm](https://github.com/btm))
- [SUSTAIN-784] Add security headers [#1427](https://github.com/chef/chef-server/pull/1427) ([nsdavidson](https://github.com/nsdavidson))
- [reconfigure] Rescue all IPAddr errors [#1425](https://github.com/chef/chef-server/pull/1425) ([stevendanna](https://github.com/stevendanna))
- Disable Welcome Page [#1428](https://github.com/chef/chef-server/pull/1428) ([lancewf](https://github.com/lancewf))
- [SUSTAIN-782] Fix permissions on gemfiles created with strict umask on reconfigure [#1431](https://github.com/chef/chef-server/pull/1431) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- [SUSTAIN-600] Make request logging configurable [#1433](https://github.com/chef/chef-server/pull/1433) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- change to test ipv6 fix [#1439](https://github.com/chef/chef-server/pull/1439) ([markan](https://github.com/markan))

## [12.17.5](https://github.com/chef/chef-server/tree/12.17.5) (2017-10-25)

#### Merged Pull Requests
- set use_implicit_hosts to false [#1408](https://github.com/chef/chef-server/pull/1408) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Release 12.17.5 [#1412](https://github.com/chef/chef-server/pull/1412) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))

## [12.17.3](https://github.com/chef/chef-server/tree/12.17.3) (2017-10-19)

#### Merged Pull Requests
- Stats endpoint [#1384](https://github.com/chef/chef-server/pull/1384) ([jaym](https://github.com/jaym))
- Removing the Gemfile.lock in an effort to move towards continuous integration [#1396](https://github.com/chef/chef-server/pull/1396) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Mitigate Host Header cache-poisoning attacks [#1398](https://github.com/chef/chef-server/pull/1398) ([stevendanna](https://github.com/stevendanna))
- Update Release_Notes for version 12.17.0 [#1399](https://github.com/chef/chef-server/pull/1399) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Update README.md [#1400](https://github.com/chef/chef-server/pull/1400) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Add config item for if pgstats will be collected [#1402](https://github.com/chef/chef-server/pull/1402) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- [pedant] Render `false` rather than `&quot;false&quot;` in template [#1404](https://github.com/chef/chef-server/pull/1404) ([stevendanna](https://github.com/stevendanna))

## [12.16.14](https://github.com/chef/chef-server/tree/12.16.14) (2017-09-21)

#### Merged Pull Requests
- Update release notes for 12.16.9 promotion [#1386](https://github.com/chef/chef-server/pull/1386) ([stevendanna](https://github.com/stevendanna))
- Correct string interpolation [#1388](https://github.com/chef/chef-server/pull/1388) ([b-dean](https://github.com/b-dean))
- Update pins to the latest ruby and rubygems [#1392](https://github.com/chef/chef-server/pull/1392) ([stevendanna](https://github.com/stevendanna))
- Fix build after ruby update [#1393](https://github.com/chef/chef-server/pull/1393) ([stevendanna](https://github.com/stevendanna))
- [SUSTAIN-632] Do not update the users table to contain the public key. The keys table should be the only source of truth [#1383](https://github.com/chef/chef-server/pull/1383) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))

## [12.16.9](https://github.com/chef/chef-server/tree/12.16.9) (2017-08-30)

#### Merged Pull Requests
- Set compression_level for RPM [#1379](https://github.com/chef/chef-server/pull/1379) ([stevendanna](https://github.com/stevendanna))
- [ctl-commands] Orphaned auth_actor cleanup tool [#1339](https://github.com/chef/chef-server/pull/1339) ([stevendanna](https://github.com/stevendanna))
- Always use compression_level 6 [#1375](https://github.com/chef/chef-server/pull/1375) ([stevendanna](https://github.com/stevendanna))
- Check for sufficient disk space before upgrade [#1381](https://github.com/chef/chef-server/pull/1381) ([stevendanna](https://github.com/stevendanna))
- [cookbooks] Make pg_upgrade timeout configurable [#1382](https://github.com/chef/chef-server/pull/1382) ([stevendanna](https://github.com/stevendanna))

## [12.16.2](https://github.com/chef/chef-server/tree/12.16.2) (2017-08-16)

#### Merged Pull Requests
- Enable chef-server in ACC [#1348](https://github.com/chef/chef-server/pull/1348) ([tduffield](https://github.com/tduffield))
- Add health checks for rabbitmq [#1345](https://github.com/chef/chef-server/pull/1345) ([jaym](https://github.com/jaym))
- [pedant] Wait for rabbitmq before every search test [#1356](https://github.com/chef/chef-server/pull/1356) ([stevendanna](https://github.com/stevendanna))
- Expose db_pool_max and db_pool_init attrbiutes for bookshelf like oc_… [#1354](https://github.com/chef/chef-server/pull/1354) ([itmustbejj](https://github.com/itmustbejj))
- Fixing reindex&#39;s use of elasticsearch scroll API [#1358](https://github.com/chef/chef-server/pull/1358) ([ksubrama](https://github.com/ksubrama))
- Fixed deleting ES indexes for ES5 and ES2 [#1367](https://github.com/chef/chef-server/pull/1367) ([lancewf](https://github.com/lancewf))
- Use NGINX cert path attributes to configure RabbitMQ [#1366](https://github.com/chef/chef-server/pull/1366) ([schisamo](https://github.com/schisamo))
- [omnibus] bump postgres versions [#1372](https://github.com/chef/chef-server/pull/1372) ([srenatus](https://github.com/srenatus))
- [erchef] Remove input validation for user info [#1343](https://github.com/chef/chef-server/pull/1343) ([stevendanna](https://github.com/stevendanna))
- Set the correct owner and permissions for SSL certificate and key [#1370](https://github.com/chef/chef-server/pull/1370) ([jaym](https://github.com/jaym))
- Revert &quot;Make root own the certs, but part of the opscode group&quot; [#1376](https://github.com/chef/chef-server/pull/1376) ([jaym](https://github.com/jaym))



## [12.15.8](https://github.com/chef/chef-server/tree/12.15.8) (2017-06-20)
[Full Changelog](https://github.com/chef/chef-server/compare/12.15.7...12.15.8)

**Fixed bugs:**

- not\_found\_message for "groups" undefined, returns HTTP 500 instead of 404 [\#1284](https://github.com/chef/chef-server/issues/1284)
- Support Self-Service Org Deletion [\#48](https://github.com/chef/chef-server/issues/48)

**Closed issues:**

- Wrong token used by Chef server to send data to Chef Automate [\#1281](https://github.com/chef/chef-server/issues/1281)
- Fresh server install on Jessie fails during client run - exception handlers [\#1178](https://github.com/chef/chef-server/issues/1178)

**Merged pull requests:**

- Bump omnibus-software pin [\#1311](https://github.com/chef/chef-server/pull/1311) ([srenatus](https://github.com/srenatus))
- \[CON-689\] issue 1308 Elasticsearch 5 Support Will Break External Solr [\#1309](https://github.com/chef/chef-server/pull/1309) ([lancewf](https://github.com/lancewf))
- Update RELEASE\_PROCESS.md and CODE\_REVIEW\_CHECKLIST.md [\#1306](https://github.com/chef/chef-server/pull/1306) ([srenatus](https://github.com/srenatus))
- Remove analytics plain text secrets when insecure\_addon\_compat false [\#1305](https://github.com/chef/chef-server/pull/1305) ([srenatus](https://github.com/srenatus))
- Add tests for chef-server-running.json [\#1304](https://github.com/chef/chef-server/pull/1304) ([stevendanna](https://github.com/stevendanna))
- \[POOL-589\] Allow user creation with blank middle name. [\#1303](https://github.com/chef/chef-server/pull/1303) ([stevendanna](https://github.com/stevendanna))
- \[pedant\] Paginate search results [\#1301](https://github.com/chef/chef-server/pull/1301) ([srenatus](https://github.com/srenatus))
- Lazily evaluate ES version detection code [\#1300](https://github.com/chef/chef-server/pull/1300) ([stevendanna](https://github.com/stevendanna))
- Fix keys for validating name fields [\#1299](https://github.com/chef/chef-server/pull/1299) ([srenatus](https://github.com/srenatus))
- \[pedant\] Add sleep to re-indexing tests [\#1298](https://github.com/chef/chef-server/pull/1298) ([stevendanna](https://github.com/stevendanna))
- \[cookbooks\] Removed unused cookbook cache config option [\#1297](https://github.com/chef/chef-server/pull/1297) ([stevendanna](https://github.com/stevendanna))
- Fix test failures caused by UTF8-data and multiple email addresses [\#1296](https://github.com/chef/chef-server/pull/1296) ([stevendanna](https://github.com/stevendanna))
- \[cookbooks\] Remove apt and yum dependencies [\#1295](https://github.com/chef/chef-server/pull/1295) ([stevendanna](https://github.com/stevendanna))
- \[POOL-589\] Add regexp for {first, middle, last, display}name [\#1294](https://github.com/chef/chef-server/pull/1294) ([srenatus](https://github.com/srenatus))
- Fix `omnibus cache missing` etc [\#1293](https://github.com/chef/chef-server/pull/1293) ([srenatus](https://github.com/srenatus))
- Upgrade PostgreSQL to 9.2.21 [\#1292](https://github.com/chef/chef-server/pull/1292) ([rhass](https://github.com/rhass))
- Elvis for oc\_erchef, oc\_bifrost, and bookshelf [\#1290](https://github.com/chef/chef-server/pull/1290) ([srenatus](https://github.com/srenatus))
- \[POOL-606\] oc-chef-pedant: improve oc\_id API test coverage [\#1289](https://github.com/chef/chef-server/pull/1289) ([srenatus](https://github.com/srenatus))
- CON-638 Experimental developer support for external ES5 [\#1287](https://github.com/chef/chef-server/pull/1287) ([lancewf](https://github.com/lancewf))
- Fix 1284: 500 returned instead of 404 for unknown groups. [\#1286](https://github.com/chef/chef-server/pull/1286) ([codeadict](https://github.com/codeadict))
- Run chef-zero integration tests using master [\#1285](https://github.com/chef/chef-server/pull/1285) ([stevendanna](https://github.com/stevendanna))
- \[POOL-608\] allow for case insensitive users search by email [\#1283](https://github.com/chef/chef-server/pull/1283) ([srenatus](https://github.com/srenatus))
- Convert data collector token when returned by chef\_secrets [\#1282](https://github.com/chef/chef-server/pull/1282) ([srenatus](https://github.com/srenatus))
- Add optional ulimit for erchef [\#1279](https://github.com/chef/chef-server/pull/1279) ([danielsdeleo](https://github.com/danielsdeleo))
- \[CLOUD-371\] Allow oc-chef-pedant chef\_server URL to be configurable [\#1278](https://github.com/chef/chef-server/pull/1278) ([ryancragun](https://github.com/ryancragun))
- Pin berkshelf-no-depselector to avoid pulling in 6.0 [\#1277](https://github.com/chef/chef-server/pull/1277) ([stevendanna](https://github.com/stevendanna))
- Add a CODE\_REVIEW\_CHECKLIST.md [\#1273](https://github.com/chef/chef-server/pull/1273) ([stevendanna](https://github.com/stevendanna))

### Components
Updated Components
* zlib (1.2.8 -> 1.2.11)
* openssl (1.0.2k -> 1.0.2l)
* rb-readline (68457ded -> fd882edc)
* veil-gem (b5748829 -> 9098dd3c)
* postgresql92 (9.2.15 -> 9.2.21)
* berkshelf-no-depselector (6016ca10 -> 6016ca10)

### Contributors
* Salim Afiune
* Kartik Null Cating-Subramanian
* Ryan Cragun
* Steven Danna
* Daniel DeLeo
* Lance Finfrock
* Ryan Hass
* Pete Higgins
* Davida Marion
* Thom May
* Dairon Medina
* Marc Paradise
* Stephan Renatus
* Blake Stier

## [12.15.7](https://github.com/chef/chef-server/tree/12.15.7) (2017-05-17)
[Full Changelog](https://github.com/chef/chef-server/compare/12.15.6...12.15.7)

**Closed issues:**

- Upgrade from Chef 11 to 12 fails in fix\_permissions stage with error 400 for every node [\#1274](https://github.com/chef/chef-server/issues/1274)
- not install chefserver in my ubuntu, when i tried to install like following below [\#1269](https://github.com/chef/chef-server/issues/1269)

**Merged pull requests:**

- \[erchef\] Fix ACL updates when actor names includes '.' [\#1275](https://github.com/chef/chef-server/pull/1275) ([stevendanna](https://github.com/stevendanna))
- \[ctl-commands\] Call reindex escript with absolute path [\#1272](https://github.com/chef/chef-server/pull/1272) ([stevendanna](https://github.com/stevendanna))
- \[pedant\] Downcase chef\_server\_uid [\#1271](https://github.com/chef/chef-server/pull/1271) ([stevendanna](https://github.com/stevendanna))
- Mp/oc id missing all.svg [\#1267](https://github.com/chef/chef-server/pull/1267) ([marcparadise](https://github.com/marcparadise))
- Grab umask for gather-logs [\#1266](https://github.com/chef/chef-server/pull/1266) ([marcparadise](https://github.com/marcparadise))
- Fix issues in postgresql preflight validator [\#1264](https://github.com/chef/chef-server/pull/1264) ([stevendanna](https://github.com/stevendanna))
- \[pedant\] Enable compliance-proxy-tests, wait for listener [\#1262](https://github.com/chef/chef-server/pull/1262) ([stevendanna](https://github.com/stevendanna))

### Components
Updated Components
* berkshelf-no-depselector (e3dd3d6f -> 6016ca10)

### Contributors
* Marc Paradise
* Steven Danna
* Sean Horn
* Bryan McLellan

## [12.15.6](https://github.com/chef/chef-server/tree/12.15.6) (2017-05-05)
[Full Changelog](https://github.com/chef/chef-server/compare/12.15.5...12.15.6)

**Merged pull requests:**

- 12.15.6 release prep [\#1259](https://github.com/chef/chef-server/pull/1259) ([marcparadise](https://github.com/marcparadise))
- Use a different user for oc\_id tests to prevent side effects [\#1258](https://github.com/chef/chef-server/pull/1258) ([marcparadise](https://github.com/marcparadise))
- \[oc-id\] Use the v0 API for all chef interactions [\#1257](https://github.com/chef/chef-server/pull/1257) ([stevendanna](https://github.com/stevendanna))


## [12.15.6](https://github.com/chef/chef-server/tree/12.15.6) (2017-05-05)

[Full Changelog](https://github.com/chef/chef-server/compare/12.15.5...12.15.6)

**Merged pull requests:**

- Use a different user for oc\_id tests to prevent side effects [\#1258](https://github.com/chef/chef-server/pull/1258) ([marcparadise](https://github.com/marcparadise))
- \[oc-id\] Use the v0 API for all chef interactions [\#1257](https://github.com/chef/chef-server/pull/1257) ([stevendanna](https://github.com/stevendanna))

### Contributors
* Marc Paradise
* Steven Danna
* Jaymala Sinha

## [12.15.5](https://github.com/chef/chef-server/tree/12.15.4) (2017-05-04)
[Full Changelog](https://github.com/chef/chef-server/compare/12.15.3...12.15.5)

**Merged pull requests:**

- \[nginx\] regression: proxied compliance requests get HTTP 404 [\#1253](https://github.com/chef/chef-server/pull/1253) ([stevendanna](https://github.com/stevendanna))

Updated Components
* berkshelf-no-depselector (dd8ec048 -> e3dd3d6f)

## [12.15.3](https://github.com/chef/chef-server/tree/12.15.2) (2017-05-03)
[Full Changelog](https://github.com/chef/chef-server/compare/12.15.0...12.15.3)

**Fixed bugs:**

- Fix regression in bookshelf preflight check [\#1246](https://github.com/chef/chef-server/pull/1246) ([stevendanna](https://github.com/stevendanna))

**Closed issues:**

- Cookbook upload missing subdirectories in libraries [\#751](https://github.com/chef/chef-server/issues/751)

**Merged pull requests:**

- Exclude ldap in -running.json if disabled. [\#1247](https://github.com/chef/chef-server/pull/1247) ([marcparadise](https://github.com/marcparadise))
- Do not return 400 for valid /users/USERNAME/\_acl/PERM calls [\#1242](https://github.com/chef/chef-server/pull/1242) ([stevendanna](https://github.com/stevendanna))


## [12.15.0](https://github.com/chef/chef-server/tree/12.15.0) (2017-04-27)
[Full Changelog](https://github.com/chef/chef-server/compare/12.14.0...12.15.0)

## 12.15.0 (2017-04-27)
### Components
New Components
* server-complete

Updated Components
* cacerts (2016-04- -> 2017-01-)
* openssl (1.0.2j -> 1.0.2k)
* omnibus-ctl (43b10417 -> 76347dd8)
* knife-opc (00a6866c -> 30b6cd85)
* knife-ec-backup (36fbecfd -> 69b50524)
* liblzma (5.2.2 -> 5.2.3)


### Contributors
* Steven Danna
* Salim Afiune
* Mark Anderson
* Marc Paradise
* Jaymala Sinha
* Ryan Cragun
* Stephen Delano
* Stephan Renatus
* Thom May

**Implemented enhancements:**

- LDAP: Synthesize a displayname from surname, givenname [\#151](https://github.com/chef/chef-server/issues/151)
- Mapping Capability Between LDAP and Chef Server 12 Attributes [\#104](https://github.com/chef/chef-server/issues/104)
- Enhance API to handle global groups in local contexts [\#1159](https://github.com/chef/chef-server/pull/1159) ([markan](https://github.com/markan))

**Closed issues:**

- Failure upgrading RabbitMQ from Migration 1.30 to 1.31 [\#1220](https://github.com/chef/chef-server/issues/1220)
- Allow setting LDAP "displayName" attribute in Chef Server [\#800](https://github.com/chef/chef-server/issues/800)
- LDAP user name mojibake [\#675](https://github.com/chef/chef-server/issues/675)

**Merged pull requests:**

- Fix Chef 12 oc\_id regression [\#1244](https://github.com/chef/chef-server/pull/1244) ([stevendanna](https://github.com/stevendanna))
- Fix "chef-server-ctl reconfigure" for ldap configuration changes [\#1240](https://github.com/chef/chef-server/pull/1240) ([jaymalasinha](https://github.com/jaymalasinha))
- add me \(sr\) to maintainers [\#1239](https://github.com/chef/chef-server/pull/1239) ([srenatus](https://github.com/srenatus))
- Return 401 for requests from the future [\#1238](https://github.com/chef/chef-server/pull/1238) ([stevendanna](https://github.com/stevendanna))
- Pull latest omnibus to fix SLES \(Suse\) x86 builds [\#1232](https://github.com/chef/chef-server/pull/1232) ([markan](https://github.com/markan))
- Tests run in proxied systems [\#1230](https://github.com/chef/chef-server/pull/1230) ([markan](https://github.com/markan))
- Pin the oc-chef-pedant chef-zero based tests to chef 12.x [\#1228](https://github.com/chef/chef-server/pull/1228) ([srenatus](https://github.com/srenatus))
- fix typo =\> fix actions\_password migration for external rmq [\#1227](https://github.com/chef/chef-server/pull/1227) ([srenatus](https://github.com/srenatus))
- \[CLOUD-306\] Add validate upstream location to nginx [\#1226](https://github.com/chef/chef-server/pull/1226) ([ryancragun](https://github.com/ryancragun))
- Update oc-id [\#1224](https://github.com/chef/chef-server/pull/1224) ([afiune](https://github.com/afiune))
- Bump nokogiri in oc\_id to 1.7.1+ [\#1223](https://github.com/chef/chef-server/pull/1223) ([stevendanna](https://github.com/stevendanna))
- Move wait-for-rabbit functionality into runit's check script [\#1222](https://github.com/chef/chef-server/pull/1222) ([srenatus](https://github.com/srenatus))
- Don't log rabbitmq password during migration 1.31 [\#1221](https://github.com/chef/chef-server/pull/1221) ([stevendanna](https://github.com/stevendanna))
- Fix Pedant to run in 12.14 world [\#1219](https://github.com/chef/chef-server/pull/1219) ([markan](https://github.com/markan))
- post 12.14 upgrades [\#1215](https://github.com/chef/chef-server/pull/1215) ([marcparadise](https://github.com/marcparadise))
- \[CLOUD-296\] Add required\_recipe endpoint [\#1214](https://github.com/chef/chef-server/pull/1214) ([ryancragun](https://github.com/ryancragun))
- Don't attempt to convert nil storage\_type [\#1211](https://github.com/chef/chef-server/pull/1211) ([stevendanna](https://github.com/stevendanna))
- don't include erts in relx releases [\#1209](https://github.com/chef/chef-server/pull/1209) ([stevendanna](https://github.com/stevendanna))
- Remove deprecated metadata [\#1207](https://github.com/chef/chef-server/pull/1207) ([thommay](https://github.com/thommay))
- Bump ruby to 2.3.4 to support newest chef-zero [\#1206](https://github.com/chef/chef-server/pull/1206) ([stevendanna](https://github.com/stevendanna))
- Remove more unused templates [\#1205](https://github.com/chef/chef-server/pull/1205) ([stevendanna](https://github.com/stevendanna))
- Cleaning up some TODOs. [\#1204](https://github.com/chef/chef-server/pull/1204) ([marcparadise](https://github.com/marcparadise))
- Remove used templates [\#1203](https://github.com/chef/chef-server/pull/1203) ([stevendanna](https://github.com/stevendanna))
- Re-enable the reindexing tests [\#1202](https://github.com/chef/chef-server/pull/1202) ([stevendanna](https://github.com/stevendanna))
- dvm - caching and minor refactor [\#1196](https://github.com/chef/chef-server/pull/1196) ([marcparadise](https://github.com/marcparadise))
- \[ldap\] Don't mangle multi-byte characters [\#1002](https://github.com/chef/chef-server/pull/1002) ([stevendanna](https://github.com/stevendanna))
- \[ldap\] Allow user customization of field mapping [\#1001](https://github.com/chef/chef-server/pull/1001) ([stevendanna](https://github.com/stevendanna))
- \[bookshelf/omnibus\] Convert storage\_type to a string consistently [\#710](https://github.com/chef/chef-server/pull/710) ([stevendanna](https://github.com/stevendanna))

## [12.14.0](https://github.com/chef/chef-server/tree/12.14.0) (2017-03-27)
[Full Changelog](https://github.com/chef/chef-server/compare/12.13.0...12.14.0)

### Components
Updated Components
* rabbitmq (3.3.4 -> 3.6.6)
* erlang (17.5 -> 18.3)
* config_guess (084a4076 -> 84f04b02)
* omnibus-ctl (b719d582 -> 43b10417)
* knife-ec-backup (2.0.6 -> 36fbecfd)
* berkshelf-no-depselector (ed797b94 -> dd8ec048)
* ohai (e5abf16c -> 5804e6da)
* chef (fcb0ccc2 -> b2ab74b7)
* veil-gem (master -> b5748829)

### Contributors
* Ameir Abdeldayem
* Irving Popovetsky
* Marc Paradise
* Mark Anderson
* Stephan Renatus
* Steven Danna

**Fixed bugs:**

- Unset/reset SVDIR inside chef-server-ctl [\#1075](https://github.com/chef/chef-server/issues/1075)

**Closed issues:**

- chef-backend-ctl create-cluster creates broken cluster if ipv6 is turned on [\#1111](https://github.com/chef/chef-server/issues/1111)
- Organizations starting with "bookshelf" are unusable [\#694](https://github.com/chef/chef-server/issues/694)
- chef-server-ctl tail: cannot follow ‘-’ by name [\#672](https://github.com/chef/chef-server/issues/672)
- chef-server-ctl test failure: Search API endpoint using POST  [\#321](https://github.com/chef/chef-server/issues/321)
- Sporatic build failures in chef\_index\_batch tests [\#631](https://github.com/chef/chef-server/issues/631)

**Merged pull requests:**

- Use `chown -R` rather than a shell glob [\#1199](https://github.com/chef/chef-server/pull/1199) ([stevendanna](https://github.com/stevendanna))
- Fix oc\_id migration, revert to old pivotal permissions [\#1198](https://github.com/chef/chef-server/pull/1198) ([stevendanna](https://github.com/stevendanna))
- Only warn about passwords in c-s.rb when it matters. [\#1197](https://github.com/chef/chef-server/pull/1197) ([marcparadise](https://github.com/marcparadise))
- Use before notification to ensure haproxy is started [\#1195](https://github.com/chef/chef-server/pull/1195) ([stevendanna](https://github.com/stevendanna))
- fix typo in external postgres validator [\#1194](https://github.com/chef/chef-server/pull/1194) ([srenatus](https://github.com/srenatus))
- make rabbitmq password migration a noop if local rabbitmq is not enabled [\#1193](https://github.com/chef/chef-server/pull/1193) ([srenatus](https://github.com/srenatus))
- Stop service before the reconfigure during upgrades [\#1192](https://github.com/chef/chef-server/pull/1192) ([stevendanna](https://github.com/stevendanna))
- Make sure chef server provision recipe runs [\#1191](https://github.com/chef/chef-server/pull/1191) ([marcparadise](https://github.com/marcparadise))
- ensure that all guests have a correct hosts file [\#1190](https://github.com/chef/chef-server/pull/1190) ([marcparadise](https://github.com/marcparadise))
- delete SVDIR for chef 11 upgrades [\#1189](https://github.com/chef/chef-server/pull/1189) ([srenatus](https://github.com/srenatus))
- Don't shortcut out of adding the webui key [\#1188](https://github.com/chef/chef-server/pull/1188) ([marcparadise](https://github.com/marcparadise))
- add pushy sql\_ro\_password [\#1187](https://github.com/chef/chef-server/pull/1187) ([srenatus](https://github.com/srenatus))
- Add reporting sql\_ro\_password to known creds, remove keepalived default [\#1186](https://github.com/chef/chef-server/pull/1186) ([srenatus](https://github.com/srenatus))
- Add a new vm named "custom" to simplify bringing up an unconfigured b… [\#1185](https://github.com/chef/chef-server/pull/1185) ([marcparadise](https://github.com/marcparadise))
- Remove preflight check for secrets [\#1184](https://github.com/chef/chef-server/pull/1184) ([stevendanna](https://github.com/stevendanna))
- Enable analytics when insecure-addon mode is enabled. [\#1183](https://github.com/chef/chef-server/pull/1183) ([marcparadise](https://github.com/marcparadise))
- Add add-ons' credentials to known credentials list [\#1182](https://github.com/chef/chef-server/pull/1182) ([srenatus](https://github.com/srenatus))
- Fix update-changelog to account for compression change [\#1180](https://github.com/chef/chef-server/pull/1180) ([stevendanna](https://github.com/stevendanna))
- Add remove-secrets command, cleanup veil invocations [\#1179](https://github.com/chef/chef-server/pull/1179) ([stevendanna](https://github.com/stevendanna))
- \[dvm\] use veil-env-helper where necessary [\#1177](https://github.com/chef/chef-server/pull/1177) ([srenatus](https://github.com/srenatus))
- Cleanup leftover secrets\_file oc-id config [\#1176](https://github.com/chef/chef-server/pull/1176) ([stevendanna](https://github.com/stevendanna))
- Set insecure\_addon\_compat to true by default [\#1174](https://github.com/chef/chef-server/pull/1174) ([stevendanna](https://github.com/stevendanna))
- Update chef & ohai pins and set log level [\#1173](https://github.com/chef/chef-server/pull/1173) ([stevendanna](https://github.com/stevendanna))
- Use the FD provider to facilitate root:root ownership of the secrets file [\#1172](https://github.com/chef/chef-server/pull/1172) ([stevendanna](https://github.com/stevendanna))
- bump license\_scout for override fix \(File::ShareDir\) [\#1171](https://github.com/chef/chef-server/pull/1171) ([srenatus](https://github.com/srenatus))
- \[oc-id\] Don't write application configuration to disk by default [\#1167](https://github.com/chef/chef-server/pull/1167) ([stevendanna](https://github.com/stevendanna))
- \[dvm\]: fixes for oc\_reporting [\#1166](https://github.com/chef/chef-server/pull/1166) ([srenatus](https://github.com/srenatus))
- Remove webui public key from disk [\#1165](https://github.com/chef/chef-server/pull/1165) ([stevendanna](https://github.com/stevendanna))
- Run oc\_id's svlogd as user opscode [\#1163](https://github.com/chef/chef-server/pull/1163) ([srenatus](https://github.com/srenatus))
- \[dvm\] Make sure to purge nodes/\*.json [\#1162](https://github.com/chef/chef-server/pull/1162) ([marcparadise](https://github.com/marcparadise))
- Use new argument format for veil-env-helper [\#1161](https://github.com/chef/chef-server/pull/1161) ([stevendanna](https://github.com/stevendanna))
- fix preflight check error message boot007, add fixture key removal to cleanup [\#1160](https://github.com/chef/chef-server/pull/1160) ([srenatus](https://github.com/srenatus))
- Mp/update other locks too [\#1158](https://github.com/chef/chef-server/pull/1158) ([marcparadise](https://github.com/marcparadise))
- Update veil gem in partybus [\#1157](https://github.com/chef/chef-server/pull/1157) ([marcparadise](https://github.com/marcparadise))
- Convert from ibrowse to httpc in oc\_chef\_wm\_containers\_SUITE [\#1156](https://github.com/chef/chef-server/pull/1156) ([stevendanna](https://github.com/stevendanna))
- bump license\_scout: it can now deal with hex pkgs [\#1155](https://github.com/chef/chef-server/pull/1155) ([srenatus](https://github.com/srenatus))
- Upgrade chef\_secrets to latest [\#1154](https://github.com/chef/chef-server/pull/1154) ([stevendanna](https://github.com/stevendanna))
- remove veil-gem software definition [\#1153](https://github.com/chef/chef-server/pull/1153) ([srenatus](https://github.com/srenatus))
- Preflight check to warn about non-FIPS openssl [\#1152](https://github.com/chef/chef-server/pull/1152) ([stevendanna](https://github.com/stevendanna))
- make os\_chef12\_upgrade use key.path for tempfile [\#1151](https://github.com/chef/chef-server/pull/1151) ([srenatus](https://github.com/srenatus))
- Use higher compression level for release builds [\#1150](https://github.com/chef/chef-server/pull/1150) ([srenatus](https://github.com/srenatus))
- Install knife-ec-backup from git [\#1149](https://github.com/chef/chef-server/pull/1149) ([stevendanna](https://github.com/stevendanna))
- fix rabbit migration: start/stop rabbitmq [\#1148](https://github.com/chef/chef-server/pull/1148) ([srenatus](https://github.com/srenatus))
- ctl command secrets access cleanup [\#1146](https://github.com/chef/chef-server/pull/1146) ([marcparadise](https://github.com/marcparadise))
- Fix pedant compare [\#1145](https://github.com/chef/chef-server/pull/1145) ([markan](https://github.com/markan))
- Use at\_exit to prevent tempfile from unlinkng too soon [\#1144](https://github.com/chef/chef-server/pull/1144) ([marcparadise](https://github.com/marcparadise))
- Add preflight check for half-disabled ipv6 systems [\#1141](https://github.com/chef/chef-server/pull/1141) ([srenatus](https://github.com/srenatus))
- Remove sensitive attributes [\#1140](https://github.com/chef/chef-server/pull/1140) ([marcparadise](https://github.com/marcparadise))
- Do not request secrets that don't exist [\#1139](https://github.com/chef/chef-server/pull/1139) ([marcparadise](https://github.com/marcparadise))
- Mp/preseed dhparam for dev [\#1138](https://github.com/chef/chef-server/pull/1138) ([marcparadise](https://github.com/marcparadise))
- Ingest S3 access key id/secret for bookshelf [\#1137](https://github.com/chef/chef-server/pull/1137) ([srenatus](https://github.com/srenatus))
- Always render bookshelf route [\#1136](https://github.com/chef/chef-server/pull/1136) ([stevendanna](https://github.com/stevendanna))
- Remove unused secret\_token template [\#1135](https://github.com/chef/chef-server/pull/1135) ([stevendanna](https://github.com/stevendanna))
- rabbitmq: add runit control script for termination [\#1134](https://github.com/chef/chef-server/pull/1134) ([srenatus](https://github.com/srenatus))
- Temporarily disable the reindex OPC test [\#1133](https://github.com/chef/chef-server/pull/1133) ([marcparadise](https://github.com/marcparadise))
- Do not configure secrets or content for analytics [\#1132](https://github.com/chef/chef-server/pull/1132) ([marcparadise](https://github.com/marcparadise))
- chef\_secrets for data collector [\#1131](https://github.com/chef/chef-server/pull/1131) ([marcparadise](https://github.com/marcparadise))
- Convert opscode-expander to using veil [\#1129](https://github.com/chef/chef-server/pull/1129) ([stevendanna](https://github.com/stevendanna))
- Store external\_rabbitmq\['actions\_password'\] in credentials store [\#1128](https://github.com/chef/chef-server/pull/1128) ([stevendanna](https://github.com/stevendanna))
- fix forgotten omnibus-software-bump \(for rabbitmq 3.6.6\) [\#1126](https://github.com/chef/chef-server/pull/1126) ([srenatus](https://github.com/srenatus))
- Upgrade RabbitMQ [\#1125](https://github.com/chef/chef-server/pull/1125) ([srenatus](https://github.com/srenatus))
- make private-chef::rabbitmq and partybus use veil [\#1123](https://github.com/chef/chef-server/pull/1123) ([srenatus](https://github.com/srenatus))
- Ingest user-supplied passwords [\#1121](https://github.com/chef/chef-server/pull/1121) ([stevendanna](https://github.com/stevendanna))
- Fix private-chef specs to work with recent veil changes [\#1120](https://github.com/chef/chef-server/pull/1120) ([srenatus](https://github.com/srenatus))
- \[oc-chef-pedant\] Don't directly depend on veil [\#1119](https://github.com/chef/chef-server/pull/1119) ([stevendanna](https://github.com/stevendanna))
- \[travis\] remove veil from on-the-flight Gemfile generation [\#1118](https://github.com/chef/chef-server/pull/1118) ([srenatus](https://github.com/srenatus))
- \[nginx\] Only access the REDIS\_PASSWORD env variable once [\#1117](https://github.com/chef/chef-server/pull/1117) ([stevendanna](https://github.com/stevendanna))
- Use veil to access webui key and secret\_key\_base in oc\_id [\#1116](https://github.com/chef/chef-server/pull/1116) ([stevendanna](https://github.com/stevendanna))
- Make rabbitmq credentials-execute resources sensitive and don't output passwords [\#1115](https://github.com/chef/chef-server/pull/1115) ([srenatus](https://github.com/srenatus))
- Do not set secrets ownership until after the owning user exists [\#1114](https://github.com/chef/chef-server/pull/1114) ([marcparadise](https://github.com/marcparadise))
- Clean up redis\_password from lua configuration [\#1113](https://github.com/chef/chef-server/pull/1113) ([stevendanna](https://github.com/stevendanna))
- Don't render redis password to redis.config [\#1110](https://github.com/chef/chef-server/pull/1110) ([stevendanna](https://github.com/stevendanna))
- add another safeguard to reindex test [\#1109](https://github.com/chef/chef-server/pull/1109) ([srenatus](https://github.com/srenatus))
- \[SPOOL-528\] update chef-server to use Veil for pivotal/webui keys [\#1107](https://github.com/chef/chef-server/pull/1107) ([marcparadise](https://github.com/marcparadise))
- \[nginx\] Don't require trailing slash on bookshelf routes [\#1106](https://github.com/chef/chef-server/pull/1106) ([stevendanna](https://github.com/stevendanna))
- travis: cache oc\_erchef plt [\#1105](https://github.com/chef/chef-server/pull/1105) ([stevendanna](https://github.com/stevendanna))
- use veil from chef secrets repo [\#1104](https://github.com/chef/chef-server/pull/1104) ([srenatus](https://github.com/srenatus))
- Remove unrendered template [\#1103](https://github.com/chef/chef-server/pull/1103) ([stevendanna](https://github.com/stevendanna))
- Wire chef\_secrets into erchef, bifrost, bookshelf [\#1101](https://github.com/chef/chef-server/pull/1101) ([srenatus](https://github.com/srenatus))
- Add a newline in case the existing config doesn't have one [\#1098](https://github.com/chef/chef-server/pull/1098) ([stevendanna](https://github.com/stevendanna))
- Fixup travis\_env target in Makefile [\#1097](https://github.com/chef/chef-server/pull/1097) ([stevendanna](https://github.com/stevendanna))
- Don't proxy to local bookshelf if it's disabled. [\#1096](https://github.com/chef/chef-server/pull/1096) ([ameir](https://github.com/ameir))
- Bump version for development [\#1095](https://github.com/chef/chef-server/pull/1095) ([stevendanna](https://github.com/stevendanna))
- Use OTP 18.3 [\#1094](https://github.com/chef/chef-server/pull/1094) ([srenatus](https://github.com/srenatus))
- \[omnibus\] Allow user to override ssl cert generation [\#1092](https://github.com/chef/chef-server/pull/1092) ([stevendanna](https://github.com/stevendanna))
- Make the ElasticSearch shard and replica count configurable [\#988](https://github.com/chef/chef-server/pull/988) ([irvingpop](https://github.com/irvingpop))

## [12.13.0](https://github.com/chef/chef-server/tree/12.13.0) (2017-02-20)
[Full Changelog](https://github.com/chef/chef-server/compare/12.12.0...12.13.0)

### Components
New Components
* erlang-crypto2 (d60d87ed)

Updated Components
* omnibus-ctl (4db34135 -> b719d582)
* berkshelf-no-depselector (f49321c9 -> ed797b94)
* rb-readline (323fed5a -> 68457ded)

### Contributors
* Rachel Adler
* Tyler Ball
* Tyler Cloke
* Steven Danna
* Marc Paradise
* Stephan Renatus

**Merged pull requests:**

- \[reindex\] Move new config items to the correct section of the config [\#1091](https://github.com/chef/chef-server/pull/1091) ([stevendanna](https://github.com/stevendanna))
- \[pedant\] Fix PATH in queues\_empty? helper, add debug output [\#1089](https://github.com/chef/chef-server/pull/1089) ([stevendanna](https://github.com/stevendanna))
- \[chef\_index\] Fix racy chef\_wait\_group test [\#1088](https://github.com/chef/chef-server/pull/1088) ([stevendanna](https://github.com/stevendanna))
- use sudo to update chef-server.rb [\#1087](https://github.com/chef/chef-server/pull/1087) ([marcparadise](https://github.com/marcparadise))
- Update omnibus-software [\#1086](https://github.com/chef/chef-server/pull/1086) ([srenatus](https://github.com/srenatus))
- Update Marc's mailmap entry [\#1085](https://github.com/chef/chef-server/pull/1085) ([stevendanna](https://github.com/stevendanna))
- \[solr4\] Clean up unused and unwanted configuration [\#1083](https://github.com/chef/chef-server/pull/1083) ([stevendanna](https://github.com/stevendanna))
- Use canned response openssl mock in oc\_chef\_wm tests [\#1082](https://github.com/chef/chef-server/pull/1082) ([srenatus](https://github.com/srenatus))
- \[erchef\] Improve reindex reliability when using ElasticSearch [\#1081](https://github.com/chef/chef-server/pull/1081) ([stevendanna](https://github.com/stevendanna))
- Make chef\_index\_batch\_tests a less flaky [\#1080](https://github.com/chef/chef-server/pull/1080) ([srenatus](https://github.com/srenatus))
- Remove the default admin page from solr4. [\#1078](https://github.com/chef/chef-server/pull/1078) ([marcparadise](https://github.com/marcparadise))
- Post-release upgrade of some dependencies [\#1077](https://github.com/chef/chef-server/pull/1077) ([srenatus](https://github.com/srenatus))
- \[ER-455\] Run pedant tests with FIPS enabled [\#1076](https://github.com/chef/chef-server/pull/1076) ([tyler-ball](https://github.com/tyler-ball))
- \[ER-459\] Ruby templating missing newline [\#1074](https://github.com/chef/chef-server/pull/1074) ([tyler-ball](https://github.com/tyler-ball))
- Testing branch for sql timeouts [\#1073](https://github.com/chef/chef-server/pull/1073) ([marcparadise](https://github.com/marcparadise))
- FIPS pipeline not building openssl-fips [\#1069](https://github.com/chef/chef-server/pull/1069) ([tyler-ball](https://github.com/tyler-ball))
- Use xz compression \(level 1\) for RPM and DEBs [\#1068](https://github.com/chef/chef-server/pull/1068) ([srenatus](https://github.com/srenatus))
- \[ER-459\] Add crypto2 to the Erlang load path in FIPS mode [\#1065](https://github.com/chef/chef-server/pull/1065) ([rmoshier](https://github.com/rmoshier))
- \[ER-444\] Enable FIPS configuration at runtime via chef-server.rb [\#1061](https://github.com/chef/chef-server/pull/1061) ([tyler-ball](https://github.com/tyler-ball))

### Contributors
* Marc Paradise
* Steven Danna
* Jaymala Sinha

## [12.12.0](https://github.com/chef/chef-server/tree/12.12.0) (2017-01-26)
[Full Changelog](https://github.com/chef/chef-server/compare/12.11.1...12.12.0)

### Components
Updated Components
* ruby (2.2.5 -> 2.2.6)
* rubygems (2.6.6 -> 2.6.8)
* omnibus-ctl (a0ccf08a -> 4db34135)
* server-jre (8u91 -> 8u121)
* knife-opc (a08237d4 -> 00a6866c)
* berkshelf-no-depselector (abb27143 -> f49321c9)
* ohai (c34212ea -> e5abf16c)
* appbundler (76cb1728 -> 6582b688)
* rb-readline (5e1c55db -> 323fed5a)
* chef_backup-gem (67b1f51a -> bc1105b7)
* chef (c21db758 -> fcb0ccc2)

### Contributors
* Steven Danna
* Stephan Renatus
* Mark Harrison
* Thom May
* Marc Paradise
* Larry Eichenbaum
* Matt Campbell
* Shadae Holmes
* Bryan McLellan
* Nathan L Smith
* Ryan Cragun
* Sean Nolen

**Fixed bugs:**

- chef-server-ctl password command does not accept special characters  [\#366](https://github.com/chef/chef-server/issues/366)
- Chef Server API should not allow usernames with spaces [\#90](https://github.com/chef/chef-server/issues/90)
- Confusing error message with duplicate email address [\#59](https://github.com/chef/chef-server/issues/59)

**Closed issues:**

- enable fips mode with  chef-server-fips-core package [\#1024](https://github.com/chef/chef-server/issues/1024)
- error in chef-server-ctl reconfigure in docker. [\#960](https://github.com/chef/chef-server/issues/960)
- Data bag search doesn't work correctly for encrypted arrays [\#876](https://github.com/chef/chef-server/issues/876)
- rebar3 warning during build [\#630](https://github.com/chef/chef-server/issues/630)
- oc\_id: email configuration [\#547](https://github.com/chef/chef-server/issues/547)
- Add settings to production.yml for sending mail [\#185](https://github.com/chef/chef-server/issues/185)

**Merged pull requests:**

- Fail if files include UTF-8 characters [\#1067](https://github.com/chef/chef-server/pull/1067) ([marcparadise](https://github.com/marcparadise))
- Log service start exit code. Use status to verify started service [\#1066](https://github.com/chef/chef-server/pull/1066) ([marcparadise](https://github.com/marcparadise))
- Bump omnibus-software to roll back to runit 2.1.1 [\#1063](https://github.com/chef/chef-server/pull/1063) ([stevendanna](https://github.com/stevendanna))
- Minor README changes [\#1062](https://github.com/chef/chef-server/pull/1062) ([stevendanna](https://github.com/stevendanna))
- Update omnibus-software to get the latest server-jre [\#1059](https://github.com/chef/chef-server/pull/1059) ([stevendanna](https://github.com/stevendanna))
- Fix the fips build [\#1056](https://github.com/chef/chef-server/pull/1056) ([stevendanna](https://github.com/stevendanna))
- Remove coverdata files from oc\_erchef dir [\#1055](https://github.com/chef/chef-server/pull/1055) ([stevendanna](https://github.com/stevendanna))
- Fix Makefiles to not distclean by default [\#1054](https://github.com/chef/chef-server/pull/1054) ([stevendanna](https://github.com/stevendanna))
- Update depselector\_rb Gemfile to use HTTPS [\#1053](https://github.com/chef/chef-server/pull/1053) ([stevendanna](https://github.com/stevendanna))
- Make oc-chef-pedant wait for queues to be empty in reindex tests [\#1051](https://github.com/chef/chef-server/pull/1051) ([srenatus](https://github.com/srenatus))
- \[SPOOL-490\] Make java handle GC log rotation [\#1050](https://github.com/chef/chef-server/pull/1050) ([srenatus](https://github.com/srenatus))
- Lock chef-client and ohai to 12.17.44 and 8.22.1 [\#1049](https://github.com/chef/chef-server/pull/1049) ([marcparadise](https://github.com/marcparadise))
- Update maintainers with recent changes [\#1048](https://github.com/chef/chef-server/pull/1048) ([marcparadise](https://github.com/marcparadise))
- update index to include the reconfigure steps... [\#1045](https://github.com/chef/chef-server/pull/1045) ([larryebaum](https://github.com/larryebaum))
- \[erchef\] Improve error message on user conflict [\#1044](https://github.com/chef/chef-server/pull/1044) ([stevendanna](https://github.com/stevendanna))
- \[oc-id\] Bump omniauth to 1.3.2 [\#1042](https://github.com/chef/chef-server/pull/1042) ([stevendanna](https://github.com/stevendanna))
- \[CON-514\] chef-server ctl install for chef reporting warning added [\#1041](https://github.com/chef/chef-server/pull/1041) ([Shadae](https://github.com/Shadae))
- Update server-jre to 8u111 [\#1040](https://github.com/chef/chef-server/pull/1040) ([stevendanna](https://github.com/stevendanna))
- Bump license\_scout, omnibus, and omnibus-software [\#1039](https://github.com/chef/chef-server/pull/1039) ([stevendanna](https://github.com/stevendanna))
- \[bookshelf\] Don't send Cache-Control headers for 404 responses [\#1038](https://github.com/chef/chef-server/pull/1038) ([stevendanna](https://github.com/stevendanna))
- Use OpenSSL::Digest instead of Digest for FIPS [\#1035](https://github.com/chef/chef-server/pull/1035) ([btm](https://github.com/btm))
- Start developer documentation [\#1033](https://github.com/chef/chef-server/pull/1033) ([stevendanna](https://github.com/stevendanna))
- Pass email address along when resetting password [\#1032](https://github.com/chef/chef-server/pull/1032) ([mivok](https://github.com/mivok))
- Ignore `-w` option on `chef-server-ctl reindex` with Elasticsearch [\#1031](https://github.com/chef/chef-server/pull/1031) ([smith](https://github.com/smith))
- Remove chef-web-downloads from RELEASE\_PROCESS [\#1030](https://github.com/chef/chef-server/pull/1030) ([stevendanna](https://github.com/stevendanna))
- replace node\[fqdn\] with server\_name in nginx.conf [\#1029](https://github.com/chef/chef-server/pull/1029) ([srenatus](https://github.com/srenatus))
- Return correct number of rows when searching with Elasticsearch [\#1028](https://github.com/chef/chef-server/pull/1028) ([smith](https://github.com/smith))
- Update docs for development environment [\#1027](https://github.com/chef/chef-server/pull/1027) ([thommay](https://github.com/thommay))
- Move to the latest omnibus-software [\#1026](https://github.com/chef/chef-server/pull/1026) ([stevendanna](https://github.com/stevendanna))
- Retry connection errors when bootstrapping bifrost [\#1025](https://github.com/chef/chef-server/pull/1025) ([ryancragun](https://github.com/ryancragun))
- Bump version for development [\#1019](https://github.com/chef/chef-server/pull/1019) ([stevendanna](https://github.com/stevendanna))
- Release note version update [\#1011](https://github.com/chef/chef-server/pull/1011) ([tas50](https://github.com/tas50))
- Accept passwords with special chars in `chef-server-ctl password` [\#1005](https://github.com/chef/chef-server/pull/1005) ([stevendanna](https://github.com/stevendanna))
- Validate user's 'name' before 'username' [\#1004](https://github.com/chef/chef-server/pull/1004) ([stevendanna](https://github.com/stevendanna))
- Added email host and from address configuration for oc\_id [\#553](https://github.com/chef/chef-server/pull/553) ([dissonanz](https://github.com/dissonanz))

## [12.11.1](https://github.com/chef/chef-server/tree/12.11.1) (2016-11-17)
[Full Changelog](https://github.com/chef/chef-server/compare/12.11.0...12.11.1)

**Merged pull requests:**

- Update omnibus-software to latest for node s390x fix [\#1017](https://github.com/chef/chef-server/pull/1017) ([smith](https://github.com/smith))
- Loosen TLS config for pushy-server 1.x [\#1016](https://github.com/chef/chef-server/pull/1016) ([stevendanna](https://github.com/stevendanna))

### Components
Updated Components
* berkshelf-no-depselector (305b4fa8 -> abb27143)
* chef (ed93e0fb -> c21db758)

### Contributors
* Nathan L Smith
* Steven Danna

## [12.11.0](https://github.com/chef/chef-server/tree/12.11.0) (2016-11-10)
[Full Changelog](https://github.com/chef/chef-server/compare/12.10.0...12.11.0)

**Merged pull requests:**

- Add AES256-GCM-SHA384 to the allowed SSL ciphers [\#1007](https://github.com/chef/chef-server/pull/1007) ([ryancragun](https://github.com/ryancragun))
- \[OPS-174\] expose compliance profiles configurables in chef-server.rb [\#1006](https://github.com/chef/chef-server/pull/1006) ([srenatus](https://github.com/srenatus))
- Add profiles forwarding to Automate endpoint [\#999](https://github.com/chef/chef-server/pull/999) ([alexpop](https://github.com/alexpop))
- remove odd git links [\#997](https://github.com/chef/chef-server/pull/997) ([thommay](https://github.com/thommay))
- Add retries to rvm install in travis [\#996](https://github.com/chef/chef-server/pull/996) ([stevendanna](https://github.com/stevendanna))
- Simplify MAINTAINERS.md [\#994](https://github.com/chef/chef-server/pull/994) ([stevendanna](https://github.com/stevendanna))
- Enable mixlib-install to resolve backwards compatible packages [\#992](https://github.com/chef/chef-server/pull/992) ([wrightp](https://github.com/wrightp))
- Set environment before exec in chef-server-ctl psql [\#991](https://github.com/chef/chef-server/pull/991) ([stevendanna](https://github.com/stevendanna))
- Bump version, update RELEASE\_PROCESS [\#990](https://github.com/chef/chef-server/pull/990) ([stevendanna](https://github.com/stevendanna))
- omnibus: Add arm build support for opscode-solr4 [\#985](https://github.com/chef/chef-server/pull/985) ([elthariel](https://github.com/elthariel))
- Security features for /data-collector/ endpoint [\#984](https://github.com/chef/chef-server/pull/984) ([sersut](https://github.com/sersut))
- Add validation endpoint to Chef Server. [\#982](https://github.com/chef/chef-server/pull/982)  ([markan](https://github.com/markan))

### Components
Updated Components
* mixlib-install (9c9dad45 -> 8622e934)
* server-jre (8u74 -> 8u91)
* berkshelf-no-depselector (03b43842 -> 305b4fa8)
* ohai (582dcc7a -> c34212ea)
* chef (fc30a44a -> b86319aa)

### Contributors
* Ryan Cragun
* Steven Danna
* Stephan Renatus
* Alex Pop
* Thom May
* Patrick Wright
* Serdar Sutay
* Mark Anderson
* Julien 'Lta' BALLET


## [12.10.0](https://github.com/chef/chef-server/tree/12.10.0) (2016-10-31)
[Full Changelog](https://github.com/chef/chef-server/compare/12.9.1...12.10.0)

**Implemented enhancements:**

- Update TLS Ciphers. [\#918](https://github.com/chef/chef-server/pull/918) ([rhass](https://github.com/rhass))

**Closed issues:**

- /etc/opscode/pivotal.pem is missing [\#986](https://github.com/chef/chef-server/issues/986)
- Disabling redirect with "non\_ssl\_port" and "enable\_non\_ssl" does not work as expected. [\#973](https://github.com/chef/chef-server/issues/973)
- Intermittent "401 Unauthorized" failures from Chef server using Chef client [\#968](https://github.com/chef/chef-server/issues/968)
- Problems creating new nodes after restoring Chef 12 server from backup [\#893](https://github.com/chef/chef-server/issues/893)
- chef server core 12.0 rc6 rpm thinks its newer than ga [\#40](https://github.com/chef/chef-server/issues/40)

**Merged pull requests:**

- Bump omnibus-software to pick up openssl version 1.0.2j. [\#981](https://github.com/chef/chef-server/pull/981) ([sersut](https://github.com/sersut))
- Remove old comment in omnibus.rb [\#980](https://github.com/chef/chef-server/pull/980) ([stevendanna](https://github.com/stevendanna))
- Use instance variables in expander.rb template [\#979](https://github.com/chef/chef-server/pull/979) ([stevendanna](https://github.com/stevendanna))
- Adds retry behavior for expander [\#978](https://github.com/chef/chef-server/pull/978) ([paulmooring](https://github.com/paulmooring))
- Pin lua to 5.1 [\#977](https://github.com/chef/chef-server/pull/977) ([stevendanna](https://github.com/stevendanna))
- Update omnibus software for latest redis and lua [\#976](https://github.com/chef/chef-server/pull/976) ([danielsdeleo](https://github.com/danielsdeleo))
- Don't pass nil to escape\_characters\_in\_string [\#974](https://github.com/chef/chef-server/pull/974) ([stevendanna](https://github.com/stevendanna))
- Don't delete ctl symlink for amazon linux [\#970](https://github.com/chef/chef-server/pull/970) ([srenatus](https://github.com/srenatus))
- Bump omnibus-software to 59e7d9 [\#967](https://github.com/chef/chef-server/pull/967) ([srenatus](https://github.com/srenatus))
- Bump omnibus software to b04ac7e2 [\#965](https://github.com/chef/chef-server/pull/965) ([danielsdeleo](https://github.com/danielsdeleo))
- Do not fail complete reindex when one object fails [\#964](https://github.com/chef/chef-server/pull/964) ([smith](https://github.com/smith))
- Simple script for analyzing disk space usage [\#963](https://github.com/chef/chef-server/pull/963) ([stevendanna](https://github.com/stevendanna))
- Update omnibus-software for latest berks & chef improvements [\#962](https://github.com/chef/chef-server/pull/962) ([stevendanna](https://github.com/stevendanna))
- a ctl-command to dump indexable data to json [\#961](https://github.com/chef/chef-server/pull/961) ([marcparadise](https://github.com/marcparadise))
- Don't include source in erlang releases [\#958](https://github.com/chef/chef-server/pull/958) ([stevendanna](https://github.com/stevendanna))
- Make loading reporting in dvm easier [\#956](https://github.com/chef/chef-server/pull/956) ([ksubrama](https://github.com/ksubrama))
- Remove nodejs from the final package [\#955](https://github.com/chef/chef-server/pull/955) ([stevendanna](https://github.com/stevendanna))
- Cleanup cached gem files in ruby install [\#954](https://github.com/chef/chef-server/pull/954) ([stevendanna](https://github.com/stevendanna))
- Bump omnibus-software [\#953](https://github.com/chef/chef-server/pull/953) ([stevendanna](https://github.com/stevendanna))
- Policy data collector [\#951](https://github.com/chef/chef-server/pull/951) ([danielsdeleo](https://github.com/danielsdeleo))
- Add cleanup build step [\#950](https://github.com/chef/chef-server/pull/950) ([stevendanna](https://github.com/stevendanna))
- Install nodejs from binary packages [\#949](https://github.com/chef/chef-server/pull/949) ([stevendanna](https://github.com/stevendanna))
- Update contributing docs [\#948](https://github.com/chef/chef-server/pull/948) ([tas50](https://github.com/tas50))
- \[omnibus\] update mixlib-install to 2.0 for PackageRouter support [\#943](https://github.com/chef/chef-server/pull/943) ([wrightp](https://github.com/wrightp))

### Components

**New Components**

* libintl-perl (1.23)
* nodejs-binary (6.7.0)
* berkshelf-no-depselector (03b43842afabd1f9c3bf127dafb1dc1dd0daba64)
* cleanup (1.0.0)

**Updated Components**
* config\_guess (9152ce40 -> 084a4076)
* openssl (1.0.1u -> 1.0.2j)
* mixlib-install (b2495ce9 -> 9c9dad45)
* openresty (1.9.7.2 -> 1.11.2.1)
* knife-opc (2ce18240 -> a08237d4)
* ohai (567dcb84 -> 582dcc7a)
* appbundler (379a06cc -> 76cb1728)
* rb-readline (cf67cd06 -> 5e1c55db)
* chef_backup-gem (cc8e5bf9 -> 67b1f51a)
* veil-gem (master -> master)
* redis (3.0.4 -> 3.0.7)
* chef (3e8b93b9 -> fc30a44a)

**Removed Components**
* bzip2 (1.0.6)
* python (2.7.9)
* nodejs (0.10.35)
* dep-selector-libgecode (1.2.0)
* berkshelf (8f7d2e1d3c8341ffe6774e92a6a136c28a0bff03)

### Contributors
* Marc Paradise
* Serdar Sutay
* Steven Danna
* Paul Mooring
* Daniel DeLeo
* Stephan Renatus
* Nathan L Smith
* Kartik Null Cating-Subramanian
* Patrick Wright
* Ryan Hass
* Tim Smith

## [12.9.1](https://github.com/chef/chef-server/tree/12.9.1) (2016-09-26)
[Full Changelog](https://github.com/chef/chef-server/compare/12.9.0...12.9.1)

**Merged pull requests:**

- Fix typo [\#941](https://github.com/chef/chef-server/pull/941) ([martinmosegaard](https://github.com/martinmosegaard))
- Fix CHEFDK\_GECODE\_PATH for latest ChefDK [\#940](https://github.com/chef/chef-server/pull/940) ([stevendanna](https://github.com/stevendanna))

### Components
Updated Components
* openssl (1.0.1t -> 1.0.1u)
* veil-gem (master -> master)
* chef (8e1312bb -> 7ed29654)

## [12.9.0](https://github.com/chef/chef-server/tree/12.9.0) (2016-09-21)
[Full Changelog](https://github.com/chef/chef-server/compare/12.8.0...12.9.0)

**Fixed bugs:**

- Make activesupport dependency less restrictive [\#930](https://github.com/chef/chef-server/pull/930) ([rhass](https://github.com/rhass))
- same name could be accessed across organizations [643](https://github.com/chef/chef-server/pull/643)
* Fixed logging LDAP password in event of some errors [156](https://github.com/chef/chef-server/issues/156)

**Closed issues:**

- chef-server 12.6.0 does not include cleanup script for opscode-expander-reindexer [\#846](https://github.com/chef/chef-server/issues/846)
- mover\_server\_admins\_global\_group\_callback: duplicate key [\#822](https://github.com/chef/chef-server/issues/822)
- erchef dumps LDAP password [\#156](https://github.com/chef/chef-server/issues/156)

**Merged pull requests:**

- Re-enable fatal licensing warnings [\#936](https://github.com/chef/chef-server/pull/936) ([sersut](https://github.com/sersut))
- License scout build fix [\#932](https://github.com/chef/chef-server/pull/932) ([marcparadise](https://github.com/marcparadise))
- Fix typos. [\#931](https://github.com/chef/chef-server/pull/931) ([ksubrama](https://github.com/ksubrama))
- Fix bug where we can update a different user during an email change [\#929](https://github.com/chef/chef-server/pull/929) ([ksubrama](https://github.com/ksubrama))
- Use recent versions of activesupport [\#928](https://github.com/chef/chef-server/pull/928) ([jkeiser](https://github.com/jkeiser))
- Don't 500 if search attribute is invalid [\#925](https://github.com/chef/chef-server/pull/925) ([tduffield](https://github.com/tduffield))
- Enable verification of email address when its changed in the profile. [\#924](https://github.com/chef/chef-server/pull/924) ([ksubrama](https://github.com/ksubrama))
- Support special characters in LDAP bind password [\#921](https://github.com/chef/chef-server/pull/921) ([tduffield](https://github.com/tduffield))
- New webmachine to fix multi-host header, and tests to cover it. [\#920](https://github.com/chef/chef-server/pull/920) ([marcparadise](https://github.com/marcparadise))
- Add LDAP server to DVM [\#919](https://github.com/chef/chef-server/pull/919) ([tduffield](https://github.com/tduffield))
- Add a 'detail=granular' query option to GET ACL endpoint [\#917](https://github.com/chef/chef-server/pull/917) ([marcparadise](https://github.com/marcparadise))
- Add plugin support to DVM [\#913](https://github.com/chef/chef-server/pull/913) ([tduffield](https://github.com/tduffield))
- Allow the ACLs endpoint to accept 'clients' and 'users' [\#912](https://github.com/chef/chef-server/pull/912) ([marcparadise](https://github.com/marcparadise))
- Add licensing information for the transitive dependencies of Chef Server [\#911](https://github.com/chef/chef-server/pull/911) ([sersut](https://github.com/sersut))
- Downgrade Ruby to 2.2.5 [\#910](https://github.com/chef/chef-server/pull/910) ([tduffield](https://github.com/tduffield))
- Update Gemfile.lock [\#909](https://github.com/chef/chef-server/pull/909) ([tduffield](https://github.com/tduffield))
- Bring in omnibus branch with mysys2 fixes [\#908](https://github.com/chef/chef-server/pull/908) ([tduffield](https://github.com/tduffield))
- Version Bumping [\#906](https://github.com/chef/chef-server/pull/906) ([tduffield](https://github.com/tduffield))
- \[SPOOL-197\] \[\#111\] clients can be added to ACL even if user exist [\#905](https://github.com/chef/chef-server/pull/905) ([marcparadise](https://github.com/marcparadise))
- SPOOL-322: Fix Elasticsearch Reindex [\#904](https://github.com/chef/chef-server/pull/904) ([tduffield](https://github.com/tduffield))
- Tweak rails DVM setup to be more like erlang [\#903](https://github.com/chef/chef-server/pull/903) ([tduffield](https://github.com/tduffield))
- Use udp\_socket\_pool\_size attribute in place of postgresql/max\_connect… [\#902](https://github.com/chef/chef-server/pull/902) ([cbalan](https://github.com/cbalan))
- Cleanup expander-reindexer from system [\#901](https://github.com/chef/chef-server/pull/901) ([tduffield](https://github.com/tduffield))
- force chef\_password:verify to fail if salt is null [\#900](https://github.com/chef/chef-server/pull/900) ([tduffield](https://github.com/tduffield))
- Secure oc-id session cookies [\#899](https://github.com/chef/chef-server/pull/899) ([tduffield](https://github.com/tduffield))
- Update dep-selector to 1.0.4 [\#896](https://github.com/chef/chef-server/pull/896) ([tduffield](https://github.com/tduffield))
- Minor logrotate configuration fixes [\#895](https://github.com/chef/chef-server/pull/895) ([mhorbul](https://github.com/mhorbul))
- Use Timeout::timeout for net/http [\#894](https://github.com/chef/chef-server/pull/894) ([thommay](https://github.com/thommay))
- Added db init and max pool size options [\#891](https://github.com/chef/chef-server/pull/891) ([paulmooring](https://github.com/paulmooring))
- Add .mailmap [\#888](https://github.com/chef/chef-server/pull/888) ([stevendanna](https://github.com/stevendanna))
- Add option to proxy insights data collector via the chef server [\#887](https://github.com/chef/chef-server/pull/887) ([stephenbm](https://github.com/stephenbm))
- if pivotal.pem \(and webui pair\) exists, ensure permissions in default recipe [\#885](https://github.com/chef/chef-server/pull/885) ([srenatus](https://github.com/srenatus))
- Ignore debs in the dev folder and fix Vagrant file [\#883](https://github.com/chef/chef-server/pull/883) ([stevendanna](https://github.com/stevendanna))
- Add pedant test coverage for 1.3 signing proto [\#882](https://github.com/chef/chef-server/pull/882) ([stevendanna](https://github.com/stevendanna))
- Make server-admins creation migration idempotent. [\#881](https://github.com/chef/chef-server/pull/881) ([tylercloke](https://github.com/tylercloke))
- Remove old jenkins build config [\#878](https://github.com/chef/chef-server/pull/878) ([stevendanna](https://github.com/stevendanna))
- \[travis\] Save rebar3 cache between builds [\#874](https://github.com/chef/chef-server/pull/874) ([stevendanna](https://github.com/stevendanna))
- Add s390x platform support for z-systems [\#869](https://github.com/chef/chef-server/pull/869) ([jaymalasinha](https://github.com/jaymalasinha))
- Fix policy\_groups policy authorization to pull from the correct org [\#643](https://github.com/chef/chef-server/pull/643) ([jkeiser](https://github.com/jkeiser))

## [12.8.0](https://github.com/chef/chef-server/tree/12.8.0) (2016-07-06)
[Full Changelog](https://github.com/chef/chef-server/compare/12.7.0...12.8.0)

**Closed issues:**

- connectivity verifier in preflight\_postgres\_validator.rb misses one possibility [\#620](https://github.com/chef/chef-server/issues/620)

**Merged pull requests:**

- \[omnibus\] Update omnibus-software for libarchive config\_guess fix [\#873](https://github.com/chef/chef-server/pull/873) ([stevendanna](https://github.com/stevendanna))
- \[omnibus\] Move from berkshelf2 to latest bookshelf [\#872](https://github.com/chef/chef-server/pull/872) ([stevendanna](https://github.com/stevendanna))
- New rack requires updating chef-zero to 4.7 [\#871](https://github.com/chef/chef-server/pull/871) ([markan](https://github.com/markan))
- Fix repo for manderson26-\>markan git change [\#870](https://github.com/chef/chef-server/pull/870) ([markan](https://github.com/markan))
- \[ET-221\] Move SAML/LDAP check into pre-flight [\#868](https://github.com/chef/chef-server/pull/868) ([chefsalim](https://github.com/chefsalim))
- \[IPO-204\] Send actions to the Data Collector before sending stats\_her… [\#867](https://github.com/chef/chef-server/pull/867) ([ryancragun](https://github.com/ryancragun))
- Fix logging in server\_admins\_existing\_users\_read\_permissions [\#866](https://github.com/chef/chef-server/pull/866) ([stevendanna](https://github.com/stevendanna))
- \[IPO-203\] Update oc\_chef\_wm to send actions to the Data Collector [\#865](https://github.com/chef/chef-server/pull/865) ([ryancragun](https://github.com/ryancragun))
- \[IPO-202\] Add initial Data Collector application and /\_status check [\#858](https://github.com/chef/chef-server/pull/858) ([ryancragun](https://github.com/ryancragun))

### Components
New Components
* libarchive (3.1.2)
* dep-selector-libgecode (1.2.0)
* berkshelf (d563dc5b5f81f62546d41dd40c43e38986bfcf75)

Updated Components
* cacerts (2016.01.20 -> 2016-04-20)
* config_guess (e39075a3 -> 5b4e8a5d)
* libxml2 (2.9.3 -> 2.9.4)
* libxslt (1.1.28 -> 1.1.29)
* ohai (d1e2fe98 -> f9992941)
* chef (de78e390 -> f5cae5ea)

Removed Components
* berkshelf2 (2.0.18)

## [12.7.0](https://github.com/chef/chef-server/tree/12.7.0) (2016-06-20)
[Full Changelog](https://github.com/chef/chef-server/compare/12.6.0...12.7.0)

**Implemented enhancements:**

- Bootstrapping a Chef server should not delete databases [\#79](https://github.com/chef/chef-server/issues/79)

**Fixed bugs:**

- oc\_id: Rails existing process detection fails and causes high CPU utlilization. [\#403](https://github.com/chef/chef-server/issues/403)
- Deleting a User Should Also Delete Any Pending Invites [\#80](https://github.com/chef/chef-server/issues/80)

**Closed issues:**

- \[chef-server-ctl\] Incorrect error messages with `user-create` [\#844](https://github.com/chef/chef-server/issues/844)

**Merged pull requests:**

- Fix whitespace in config [\#851](https://github.com/chef/chef-server/pull/851) ([jkeiser](https://github.com/jkeiser))
- Update misleading filename error message [\#862](https://github.com/chef/chef-server/pull/862) ([MichaelPereira](https://github.com/MichaelPereira))
- Add ci/run\_tests.sh to drive the CI process [\#859](https://github.com/chef/chef-server/pull/859) ([jkeiser](https://github.com/jkeiser))
- \[ET-202\] Fix chef\_manage node attribute access [\#856](https://github.com/chef/chef-server/pull/856) ([srenatus](https://github.com/srenatus))
- Update openresty to point to ppc64 lua location [\#855](https://github.com/chef/chef-server/pull/855) ([scotthain](https://github.com/scotthain))
- \[ET-202\] Check for SAML enablement during reconfigure [\#854](https://github.com/chef/chef-server/pull/854) ([chefsalim](https://github.com/chefsalim))
- Updated omnibus software pinning to pick up ppc64 friendly defs [\#853](https://github.com/chef/chef-server/pull/853) ([scotthain](https://github.com/scotthain))
- oc\_erchef users list: allow filtering by external\_authentication\_id [\#852](https://github.com/chef/chef-server/pull/852) ([sdelano](https://github.com/sdelano))
- use chef\_zero mode in vagrant for dvm [\#850](https://github.com/chef/chef-server/pull/850) ([sdelano](https://github.com/sdelano))
- Use enterprise cookbook version that supports systemd on ubuntu 16.04 [\#848](https://github.com/chef/chef-server/pull/848) ([yzl](https://github.com/yzl))
- Reset initialization\_options and vendor\_class after a chef\_run [\#841](https://github.com/chef/chef-server/pull/841) ([ryancragun](https://github.com/ryancragun))
- Add chef-server-ctl require-credential-rotation command [\#840](https://github.com/chef/chef-server/pull/840) ([ryancragun](https://github.com/ryancragun))
- Update to pick up latest omnibus and omnibus software [\#839](https://github.com/chef/chef-server/pull/839) ([mmzyk](https://github.com/mmzyk))
- Remove chef-sync from the known add on packages for the install command [\#838](https://github.com/chef/chef-server/pull/838) ([mmzyk](https://github.com/mmzyk))
- release process updates [\#836](https://github.com/chef/chef-server/pull/836) ([patrick-wright](https://github.com/patrick-wright))
- \[omnibus\] bypass\_bootstrap? should ensure both creds exist [\#835](https://github.com/chef/chef-server/pull/835) ([stevendanna](https://github.com/stevendanna))
- Add Ryan Cragun as a Chef Server maintainer [\#834](https://github.com/chef/chef-server/pull/834) ([ryancragun](https://github.com/ryancragun))
- Fixing pedant/bookshelf when nginx on non-standard port [\#833](https://github.com/chef/chef-server/pull/833) ([adamleff](https://github.com/adamleff))
- Update opscode-solr4 JAVA\_OPTS to include whitespace [\#830](https://github.com/chef/chef-server/pull/830) ([bigbam505](https://github.com/bigbam505))
- Update chef-server release process documentation. [\#829](https://github.com/chef/chef-server/pull/829) ([rmoshier](https://github.com/rmoshier))
- Release Process Updates [\#828](https://github.com/chef/chef-server/pull/828) ([schisamo](https://github.com/schisamo))
- Add support for service credentials rotation [\#798](https://github.com/chef/chef-server/pull/798) ([ryancragun](https://github.com/ryancragun))
- Updated Copyright and URL [\#771](https://github.com/chef/chef-server/pull/771) ([jjasghar](https://github.com/jjasghar))

### Components
New Components
* veil-gem (master)

Updated Components
* config_guess (706fbe57 -> ddd7f330)
* openssl (1.0.1s -> 1.0.1t)
* omnibus-ctl (e75976be -> a0ccf08a)
* sqitch (0.973 -> 0.973)
* ohai (780f7c5f -> 17e5c748)
* chef (b94e2ef4 -> f0caa91e)


### Contributors
* Brent Montague
* Michael Pereira

## [12.6.0](https://github.com/chef/chef-server/tree/12.6.0) (2016-04-29)
[Full Changelog](https://github.com/chef/chef-server/compare/12.5.0...12.6.0)

**Closed issues:**

- chef-server-ctl grant-server-admin-permissions needs cli help [\#806](https://github.com/chef/chef-server/issues/806)
- chef-server-ctl cannot load such file -- chef/key \(LoadError\) [\#632](https://github.com/chef/chef-server/issues/632)

**Merged pull requests:**

- Revert changes to sqitch plan files to avoid upgrade breakage [\#826](https://github.com/chef/chef-server/pull/826) ([stevendanna](https://github.com/stevendanna))
- \[travis\] Fixup GECODE\_PATH in travis config [\#823](https://github.com/chef/chef-server/pull/823) ([stevendanna](https://github.com/stevendanna))
- Dynamically generate the wait-for-rabbit script [\#821](https://github.com/chef/chef-server/pull/821) ([adamleff](https://github.com/adamleff))
- \[oc-chef-pedant\] Tag multiuser tests as multi-user [\#819](https://github.com/chef/chef-server/pull/819) ([stevendanna](https://github.com/stevendanna))
- Bug fix: treat a successful PG conn and auth as a preflight success [\#818](https://github.com/chef/chef-server/pull/818) ([adamleff](https://github.com/adamleff))
- Fix bug where requestor membership of public\_key\_read\_access was not being properly tested for keys access. [\#817](https://github.com/chef/chef-server/pull/817) ([tylercloke](https://github.com/tylercloke))
- \[omnibus\] Expose haproxy config in chef-server.rb [\#816](https://github.com/chef/chef-server/pull/816) ([stevendanna](https://github.com/stevendanna))
- getchef.com and opscode.com -\> chef.io [\#815](https://github.com/chef/chef-server/pull/815) ([jkeiser](https://github.com/jkeiser))
- \[oc\_id\] Set HOME in oc\_id's runsv script [\#814](https://github.com/chef/chef-server/pull/814) ([stevendanna](https://github.com/stevendanna))
- \[expander\] Set HOME in expander's runsv script [\#811](https://github.com/chef/chef-server/pull/811) ([stevendanna](https://github.com/stevendanna))
- \[omnibus\] Add rb-readline to the build [\#809](https://github.com/chef/chef-server/pull/809) ([stevendanna](https://github.com/stevendanna))
- Use HAProxy to route Postgresql and ElasticSearch connections [\#808](https://github.com/chef/chef-server/pull/808) ([stevendanna](https://github.com/stevendanna))
- Pick up latest omnibus/omnibus-software [\#805](https://github.com/chef/chef-server/pull/805) ([schisamo](https://github.com/schisamo))
- Fix error in error handling for server admins permission migration [\#804](https://github.com/chef/chef-server/pull/804) ([paulmooring](https://github.com/paulmooring))
- Work to support chef-server on IBM POWER platforms [\#797](https://github.com/chef/chef-server/pull/797) ([edolnx](https://github.com/edolnx))
- Use mixlib-installs’ built-in platform detection during add-on install [\#796](https://github.com/chef/chef-server/pull/796) ([schisamo](https://github.com/schisamo))
- Update RELEASE\_PROCESS.md to reflect the new announcement process [\#795](https://github.com/chef/chef-server/pull/795) ([mmzyk](https://github.com/mmzyk))
- Modernize Add-On Install [\#794](https://github.com/chef/chef-server/pull/794) ([schisamo](https://github.com/schisamo))
- Update upgrade docs in RELEASE\_PROCESS.md. [\#789](https://github.com/chef/chef-server/pull/789) ([tylercloke](https://github.com/tylercloke))

### Components
New Components
* mixlib-install (b2495ce9db896ce8c9c0444282e67da9d5a62a7b)
* rb-readline (cf67cd06ae89e8b2710ba930c3015639240ac7b7)
* haproxy (1.6.4)

Updated Components
* config_guess (bb8fb402 -> 706fbe57)
* rubygems (2.4.5 -> 2.4.5)
* libossp-uuid (1.6.2 -> 1.6.2)
* ohai (218d894f -> 780f7c5f)
* appbundler (c6193c09 -> a8376ff7)
* chef_backup-gem (bd29c56a -> a402a2ef)
* chef (e9194179 -> b94e2ef4)

### Contributors
* Steven Danna
* Paul Mooring
* Adam Leff
* John Keiser
* Tyler Cloke
* Seth Chisamore
* Carl Perry
* Kartik Null Cating-Subramanian
* mmzyk

## [12.5.0](https://github.com/chef/chef-server/tree/12.5.0) (2016-03-22)
[Full Changelog](https://github.com/chef/chef-server/compare/12.4.1...12.5.0)

**Fixed bugs:**

- chef-manage doesn't load chef-server node attributes \(doesn't inherit fqdn\) [\#744](https://github.com/chef/chef-server/issues/744)
- Using public EC2 name for manage + API exceeds nginx hash bucket size [\#743](https://github.com/chef/chef-server/issues/743)

**Closed issues:**

- PUT method  /organizations/NAME/node/NAME reset automatic attributes. [\#783](https://github.com/chef/chef-server/issues/783)
- 500 error after upgrade [\#762](https://github.com/chef/chef-server/issues/762)
- ubuntu\_supported\_codenames should include trusty instead of natty [\#759](https://github.com/chef/chef-server/issues/759)
- Where can I edit the hostname used by Chef Server? [\#752](https://github.com/chef/chef-server/issues/752)
- service postgresql is running externally and cannot be managed [\#733](https://github.com/chef/chef-server/issues/733)
- 502 errors from nginx while reaching erchef server [\#732](https://github.com/chef/chef-server/issues/732)
- oc-bifrost-pedant not merged into the repository [\#670](https://github.com/chef/chef-server/issues/670)
- Chef Server 12: View Public Keys of all Users, including clients [\#649](https://github.com/chef/chef-server/issues/649)
- Instructions are incorrect after installing a package during upgrade [\#646](https://github.com/chef/chef-server/issues/646)

**Merged pull requests:**

- Update inet interface [\#788](https://github.com/chef/chef-server/pull/788) ([tylercloke](https://github.com/tylercloke))
- Spool 106/update rails version [\#787](https://github.com/chef/chef-server/pull/787) ([ksubrama](https://github.com/ksubrama))
- Set missing multiuser tags; fix nil validator w/ default org [\#786](https://github.com/chef/chef-server/pull/786) ([danielsdeleo](https://github.com/danielsdeleo))
- Standardize license information based on omnibus best practices. [\#784](https://github.com/chef/chef-server/pull/784) ([sersut](https://github.com/sersut))
- \[omnibus\] Change pgsql's local service user and db superuser to not be hardcoded [\#782](https://github.com/chef/chef-server/pull/782) ([andy-dufour](https://github.com/andy-dufour))
- \[oc\_erchef\] Make the \_status endpoints health check timeout configurable. [\#781](https://github.com/chef/chef-server/pull/781) ([andy-dufour](https://github.com/andy-dufour))
- \[erchef,bifrost,chef-mover\] Update stats\_hero and other deps [\#780](https://github.com/chef/chef-server/pull/780) ([stevendanna](https://github.com/stevendanna))
- Add trusty and remove natty add on support. [\#778](https://github.com/chef/chef-server/pull/778) ([tylercloke](https://github.com/tylercloke))
- Add some more testing related info to README [\#777](https://github.com/chef/chef-server/pull/777) ([ksubrama](https://github.com/ksubrama))
- Include license information for chef-server and dependencies in omnibus packages [\#775](https://github.com/chef/chef-server/pull/775) ([sersut](https://github.com/sersut))
- Bump bundler install for chef-zero Travis to 1.10.6. [\#774](https://github.com/chef/chef-server/pull/774) ([tylercloke](https://github.com/tylercloke))
- \[chef-server-ctl\] Fix several bugs in chef-server-ctl backup [\#770](https://github.com/chef/chef-server/pull/770) ([ryancragun](https://github.com/ryancragun))
- Added /orgs/org/users/user/keys(/key) endpoint and changed default perms on org scoped key GETs.
 [\#769](https://github.com/chef/chef-server/pull/769) ([tylercloke](https://github.com/tylercloke))

```
	The following endpoints' GET methods can now be accessed by any requestor that is a member of the same organization:
	/organizations/:org/clients/:client/keys
	/organizations/:org/clients/:client/keys/:key
	/organizations/:org/users/:user/keys
	/organizations/:org/users/:user/keys/:key

	The above org-scoped user keys endpoints are new and access to them can be controlled by an admin by editing memebership
	of the public_key_read_access group.
```

- \[cookbooks\] Use only\_if resource attribute, fixing typo [\#767](https://github.com/chef/chef-server/pull/767) ([stevendanna](https://github.com/stevendanna))
- Added GET /groups/:group/transitive\_member/actors/:actor endpoint for checking recursive membership. [\#766](https://github.com/chef/chef-server/pull/766) ([tylercloke](https://github.com/tylercloke))
- Change the text on the homepage to refer to chef-manage instead [\#765](https://github.com/chef/chef-server/pull/765) ([juliandunn](https://github.com/juliandunn))
- \[omnibus\] Don't build rebar2, we don't use it [\#764](https://github.com/chef/chef-server/pull/764) ([stevendanna](https://github.com/stevendanna))
- Updated contributor doc to note that a rebase is needed before merging. [\#763](https://github.com/chef/chef-server/pull/763) ([tylercloke](https://github.com/tylercloke))
- Pull oc-bifrost-pedant in and fix base\_url bifrost bug. [\#761](https://github.com/chef/chef-server/pull/761) ([tylercloke](https://github.com/tylercloke))
- Update rubocop definition to prevent errors. [\#758](https://github.com/chef/chef-server/pull/758) ([elliott-davis](https://github.com/elliott-davis))
- Bump omnibus-software to pick up latest server-jre [\#757](https://github.com/chef/chef-server/pull/757) ([scottopherson](https://github.com/scottopherson))
- \['private\_chef'\]\['rabbitmq'\]\['management\_enabled'\] should be respected [\#756](https://github.com/chef/chef-server/pull/756) ([jmink](https://github.com/jmink))
- \[omnibus\] Remove old access\_by\_lua nginx config and allow custom acce… [\#754](https://github.com/chef/chef-server/pull/754) ([ryancragun](https://github.com/ryancragun))
- Make org creation optional in APIv1 spec [\#753](https://github.com/chef/chef-server/pull/753) ([danielsdeleo](https://github.com/danielsdeleo))
- Tag tests that expect 400 return w/ `validation` [\#747](https://github.com/chef/chef-server/pull/747) ([danielsdeleo](https://github.com/danielsdeleo))
- move hash\_bucket\_size to correct place in config file [\#746](https://github.com/chef/chef-server/pull/746) ([jamesc](https://github.com/jamesc))
- Increase default nginx server\_names\_hash\_bucket\_size to 128 from 64 [\#745](https://github.com/chef/chef-server/pull/745) ([jamesc](https://github.com/jamesc))
- Add logic to support configure yum repos for Amazon Linux \(\#741\) [\#742](https://github.com/chef/chef-server/pull/742) ([jamesc](https://github.com/jamesc))
- Split keys\_spec.rb into user\_, client\_keys\_spec.rb [\#740](https://github.com/chef/chef-server/pull/740) ([jrunning](https://github.com/jrunning))
- Bumping private-chef's enterprise cookbook dependency to 0.10.0 [\#737](https://github.com/chef/chef-server/pull/737) ([andy-dufour](https://github.com/andy-dufour))
- Upgrade Posgtresql to 9.2.15. [\#735](https://github.com/chef/chef-server/pull/735) ([rhass](https://github.com/rhass))
- Updating gatling-rsync configuration [\#734](https://github.com/chef/chef-server/pull/734) ([dmccown](https://github.com/dmccown))
- \[chef-server-ctl\] Cleanse bookshelf database when storage\_type is sql [\#729](https://github.com/chef/chef-server/pull/729) ([stevendanna](https://github.com/stevendanna))
- \[bookshelf\] Only do disk-related startup tasks in filesystem-mode [\#728](https://github.com/chef/chef-server/pull/728) ([stevendanna](https://github.com/stevendanna))
- Only define LINE\_SEP on first load [\#725](https://github.com/chef/chef-server/pull/725) ([stevendanna](https://github.com/stevendanna))
- bifrost and bookshelf schemas explicit upgrade [\#715](https://github.com/chef/chef-server/pull/715) ([marcparadise](https://github.com/marcparadise))
- Fixing upgrade instructions in package postinstall script [\#689](https://github.com/chef/chef-server/pull/689) ([andy-dufour](https://github.com/andy-dufour))
- Update rspec-rerun to latest to get rid of backtrace issues [\#664](https://github.com/chef/chef-server/pull/664) ([jkeiser](https://github.com/jkeiser))

### Components
New Components
* config_guess (bb8fb4029563dcd564ece143ce558ea44c720a15)

Updated Components
* cacerts (2014.08. -> 2016.01.)
* openssl (1.0.1q -> 1.0.1s)
* pcre (8.31 -> 8.38)
* openresty (1.9.3.1 -> 1.9.7.2)
* postgresql92 (9.2.14 -> 9.2.15)
* server-jre (8u31 -> 8u74)
* nodejs (0.10.10 -> 0.10.35)
* libxml2 (2.9.2 -> 2.9.3)
* ohai (237129a0 -> 218d894f)
* appbundler (0.6.0 -> c6193c09)
* chef_backup-gem (0.0.1.de -> bd29c56a)
* chef (22d700e4 -> e9194179)

Removed Components
* rebar (1c98f6ccd4adc915167d4302d732d79e4da3d390)


## [12.4.1](https://github.com/chef/chef-server/tree/12.4.1) (2016-02-03)
[Full Changelog](https://github.com/chef/chef-server/compare/12.4.0...12.4.1)

### Components

#### Updated
* ohai (81f1c968 -> d9262d06)
* chef (ec5a8925 -> 09227432)

## Detailed Change Log

**Fixed bugs:**

- chef-server-ctl upgrade broken in 12.4.0 [\#724](https://github.com/chef/chef-server/issues/724)
- Create cookbook artifacts with all fields filled in [\#714](https://github.com/chef/chef-server/pull/714) ([danielsdeleo](https://github.com/danielsdeleo))

## 12.4.0 (2016-01-27)

### Components

#### Updated
* openssl (1.0.1p -> 1.0.1q)
* knife-opc (528be923 -> 0b8fa0fa)
* ohai (f1e35bf1 -> 81f1c968)
* chef (2fe875ce -> 3f3fbc8f)

#### New
* rest-client (1.8.0)

#### Removed
* chef-server-bootstrap

## Detailed Change Log

* `oc-pedant`
    * Replace /policies/:group/:name in spec descriptions with /policy_groups/:group_name/policies/:policy_name.
    * Fix spec descriptions that were copied from /cookbooks to cookbook_artifacts.
    * Allow opt-out of RVM/bundler busting in knife pedant tests
    * Add validation tag to header validation test

* `oc-erchef`
    * Added ACL endpoints for policies and policy groups; also pedant tests
    * Implement RFC 14 - Add universe endpoint
    * V1 of Server Admins. Implements flexable user management global group.

* `chef-server-ctl`
    * Make sure chef-server-ctl install can do chef-manage

* `knife`
    * Add test for knife-opc org creation
    * Use validation for knife opc instead of knife

* `updated RAML documentation`

* `chef-server`
    * Restrict 'other' permissions for chef-server.rb as it may contain secrets.
    * Remove other permissions on existing copies of chef-server.rb to protect potentially sensitive config options

* `omnibus`
    * EcPostgres can be used with other databases
    * Move bootstrap to recipe/library.
    * Remove chef-server-bootstrap project
    * Create a consolidated cleanup recipe
    * Bootstrap preflight checks to prevent multiple bootstraps
    * Modify postgres preflight checks to have correct assumptions
    * Fix statem test output formatting

* `rabbitmq`
    * Correct handling of no rabbitmq in controls endpoint
    * Set rabbitmq_management listener IP to rabbitmq node_ip_address
    * Don't monitor rabbit queue length w/ actions disabled
    * Remove unused jobs queue from rabbitmq setup

* `bookshelf`
    * Support optionally storing cookbook data in postgresql rather than on the filesystem directly. This is an experimental feature and is off by default. This is only supported for new installs at this time; there is no support for migrating cookbook data from the filesystem to sql (or back).
    * Remove `bksw_sync` module

* `opscode-expander-reindexer`
    * Remove opscode-expander-reindexer service

# 12.3.0 (2015-11-12)

### Components

#### Updated
* ncurses (5.9-2015 -> 5.9)
* rubygems (1.8.24 -> 2.4.5)
* bundler (1.5.3 -> 1.10.6)
* openresty (1.7.10.1 -> 1.9.3.1)
* postgresql92 (9.2.10 -> 9.2.14)
* liblzma (5.0.5 -> 5.2.2)
* ohai (ffd9a0a0 -> c9787b96)
* appbundler (0.4.0 -> 0.6.0)
* redis (2.8.21 -> 3.0.4)
* opscode-solr4 (4.9.1 -> 4.10.4)
* chef (ad8fd4d6 -> b0dbe243)

#### New
* pkg-config-lite (0.28-1)

#### Removed
* pkg-config (0.28)
* gdbm (1.9.1)

## Detailed Change Log

* `omnibus` [616](https://github.com/chef/chef-server/pull/616) - omnibus-software-bump
  * Remove dependency on gdbm
* `oc-chef-pedant` [615](https://github.com/chef/chef-server/pull/615) - mark-more-validations
  * Mark `policy/policy` group validation specs with `:validation`.
* `oc-chef-pedant` [614](https://github.com/chef/chef-server/pull/614) - pedant\_add\_seed\_option
  * Rspec by default runs tests in a random order, which normally is
    good. However sometimes bugs manifest themselves as state leftover from
    prior tests, and it's hard to sort those out when the order changes
    every time.
    Add a --seed flag to pedant to set the rspec seed value.
* `chef-mover` [613](https://github.com/chef/chef-server/pull/613) - no-eunit-on-vendored-code
  * do not run intermittently failing tests on dependencies
    that we can't change for backward-compatibility reasons.
* `omnibus` [611](https://github.com/chef/chef-server/pull/611) - no-etc-for-erl
  * Don't create a few unused directories on new
    installs
* `omnibus` [612](https://github.com/chef/chef-server/pull/612) - migration-26-rename
  * Follow filename convention for migration `26`
* `dvm` [610](https://github.com/chef/chef-server/pull/610) - custom-dotfile-location
  * add support for dotfiles external to the repository
* `dvm` [609](https://github.com/chef/chef-server/pull/609) - dp\_add\_reporting\_template
  * missing template from [https://github.com/chef/chef-server/pull/608](https://github.com/chef/chef-server/pull/608)
* `internal-doc`, `dvm` [608](https://github.com/chef/chef-server/pull/608) - dp\_external\_pgsql\_dvm
  * allow dvm to create an external reporting db vm
* `oc-id` [606](https://github.com/chef/chef-server/pull/606) - dp\_nil\_username\_ocid
  * nil username breaks Analytics login
* `omnibus` [597](https://github.com/chef/chef-server/pull/597) - fixes584
  * [chef-server/584](https://github.com/chef/chef-server/issues/584) Adding 3 retries will ensure `bootstrap-platform` script does not fail because bifrost component slow to start up.
* `omnibus` [fix-warn](https://github.com/chef/chef-server/commit/1128fda0db9a38cb664b5e400feecbe2f459d611)
  * Fixes Chef 13 warning related to using 'environment' attribute to configure 'PATH'.
* `omnibus` [RyanFrantz-master](https://github.com/chef/chef-server/commit/a50470c41d0ee9d716f860a8d6f79cc14fde5ddd)
  * the nginx `nginx_status` endpoint is now available.
  * Sensibe defaults are defined in `attributes/default`.rb.
* `omnibus` [571](https://github.com/chef/chef-server/pull/571) - CVE-2014-3628
  * Need the md5sum too...
  * Bump to Solr 4.10.4 for CVE-2014-3628
* `dvm`, `bifrost` [588](https://github.com/chef/chef-server/pull/588) - dvm-fixes
  * dvm fixes to fix unhelpful error messages and enable successful loading of bifrost.
* `oc-chef-pedant` [600](https://github.com/chef/chef-server/pull/600) - tag-pedant-validations
  * Mark every spec expecting a `400` as `:validation`.
* `bookshelf`, `bifrost`, `erchef` [592](https://github.com/chef/chef-server/pull/592) - rebar-lock-updates-and-webmachine-rehome
  * pull in the latest
    webmachine and mochiweb dependencies to resolve an issue which could
    lead to requests being rejected under sudden load.
* `omnibus`, `erchef` [591](https://github.com/chef/chef-server/pull/591) - dp\_queue\_mon\_affects\_overall\_status
  * queue monitor doesn't affect `overall_status` by default
* `oc-chef-pedant`, `omnibus`, `erchef` [589](https://github.com/chef/chef-server/pull/589) - fcs
  * Chef Server now supports Elasticsearch as a search
    indexing backend in addition to solr.
  * Once an ElasticSearch node is configured, you can
* `omnibus`, `erchef` [570](https://github.com/chef/chef-server/pull/570) - dp\_rabbit\_monitoring
  * enable RabbitMQ Management Plugin
* `oc-id` [560](https://github.com/chef/chef-server/pull/560) - add-ocid-email
  * fixing specs
  * update `omniauth-chef` to 0.2.0
  * I18n changes
  * Changes to allow username for password changes
* `omnibus` [555](https://github.com/chef/chef-server/pull/555) - gather-log-updates
  * `gather-logs` updates
* `oc-id` [563](https://github.com/chef/chef-server/pull/563) - oc-id-hosted-copy
  * Just call it "Chef account" and "Chef username" and put it into the `i18n`
    config.
* `omnibus` [579](https://github.com/chef/chef-server/pull/579) - chef-server-ctl-proxy
  * This configuration file is used by `chef-server-ctl` to talk to the API
    locally. Proxy configs in the users environment often cause problems
    because the LB VIP is almost always `127.0.0.1`, which causes the proxy to
    try to connect to itself rather than back to the `chef-server`.
* `chef-mover` [569](https://github.com/chef/chef-server/pull/569) - be-quiet-mover
  * This test is noisy and fails at random on Travis.  It is part of
    `chef-mover`'s vendored copy of depsolver.
* `dvm` [573](https://github.com/chef/chef-server/pull/573) - fix-package-listing
  * Other parts of the installer selection code assume that the user
    gave us a number starting from 1.
* `dvm` [574](https://github.com/chef/chef-server/pull/574) - dvm-sync-cleanup
  * updated the sync tool with more configuration
    options and more succinct output.
* `omnibus` [master](https://github.com/chef/chef-server/commit/d9ed3b0c926079e56731a8dec58c0e4f493f83a8)
  * This upgrades PostgreSQL to the current release and addreses several
    CVEs.
    [http://www.postgresql.org/docs/9.2/static/release-9-2-11.html](http://www.postgresql.org/docs/9.2/static/release-9-2-11.html)
    [http://www.postgresql.org/docs/9.2/static/release-9-2-14.html](http://www.postgresql.org/docs/9.2/static/release-9-2-14.html)
* `omnibus`, `bootstrap` [545](https://github.com/chef/chef-server/pull/545) - fix-non-default-postgres-port
  * Issue `459:` Use configured port everywhere we talk to postgres
* `dvm` [566](https://github.com/chef/chef-server/pull/566) - dvm-powerdown-ssh-fail
  * It's now possible to specify AUTOPACKAGE=x where
    x is the number of the selection you'd type in. This saves the arduous
    task of having to wait for the package menu and type a number on
    `vagrant up`
  * do not check if project path is available until
    we try to load that project.
* `omnibus` [565](https://github.com/chef/chef-server/pull/565) - master
  * Change the name to be more meaningful
  * Adding configurability for erchef and bifrost logging messages per second
* `dvm` [556](https://github.com/chef/chef-server/pull/556) - warn-for-external-projects
  * Warns rather than fail if external project isn't linked
* `oc-chef-pedant` [552](https://github.com/chef/chef-server/pull/552) - search-poll-correctly
  * `with_search_polling` works by retrying when an exception is raised. An
    empty response from search will not raise an exception, rather the
    assertions on the results should also be inside the `with_search_polling`
    block.
* `omnibus` [550](https://github.com/chef/chef-server/pull/550) - fix-bundler
  * Override bundler from `omnibus-software` default of 1.5.3 to 1.10.6.
* `dvm` [reporting-updates](https://github.com/chef/chef-server/commit/a4ee35619bdf1afcf4f73146ed968064ee2d9d75)
  * add support for `oc-reporting-pedant`
  * fix dep loading that broke with rebar changes, add reporting projects, and more!
* `omnibus`, `erchef` [540](https://github.com/chef/chef-server/pull/540) - ldap-case-sensitive
  * Fix bug where logins via LDAP failed because of case
    sensitivity.
* `omnibus`, `oc-id` [543](https://github.com/chef/chef-server/pull/543) - oc-id-favicon
  * remove `oc-id` favicon
  * Uses the favicon from [https://www.chef.io/favicon.ico.](https://www.chef.io/favicon.ico.)
    `oc-id` had a blank file in that place, while the static files did not
    have one. Adding the files and the configuration to let nginx serve it.
* `omnibus` [537](https://github.com/chef/chef-server/pull/537) - backup\_exit
  * [chef-server/534](https://github.com/chef/chef-server/issues/534) Fix `chef-server-ctl` backup always returning 1
* `erchef` [541](https://github.com/chef/chef-server/pull/541) - fix-conn-leak
  * Fix HTTP `500s` generated by request timeouts to bifrost
    on `high-traffic` Chef Servers.
* `omnibus` [524](https://github.com/chef/chef-server/pull/524) - cleanup-static-nginx-files
  * Make the default index.html message more informative.
  * delete unused javsacript files from nginx deploy
* `omnibus` [536](https://github.com/chef/chef-server/pull/536) - master
  * Don't consider `opscode-chef-mover` or any other hidden service status when checking `ha-status`. This is based on assumption the `opscode-chef-mover` service is only used during an upgrade, and does not need to be running all of the time.
* `erchef` [528](https://github.com/chef/chef-server/pull/528) - spurious-status-400s
  * Fix bug where persistent clients would receive HTTP `400`
    after successful calls to the /_status endpoint.
* `erchef` [529](https://github.com/chef/chef-server/pull/529) - remove-chef-otto
  * Remove unused `chef_otto.hrl`
* `erchef` [533](https://github.com/chef/chef-server/pull/533) - stablize-batch-tests
  * A number of timeouts we were seeing seems to be a race condition in
    shutting down the `gen_server`. To avoid this, we monitor the `gen_server`
    pid and wait to get notified of its exit.
* `erchef` [532](https://github.com/chef/chef-server/pull/532) - efast\_xs
  * Only try to index `policy_name` and `policy_group` if their values are not undefined.
  * Added throw to `chef_index_expander:expand/3` when key passed with undefined value.
  * Added `efast_xs` to relx section of rebar.config.
* `dvm` [516](https://github.com/chef/chef-server/pull/516) - forward-ssh-agent
  * Occasionally we want to clone private repositories inside the `dev-vm`.
    The forwarded `ssh-agent` makes this easier since the user can add their
    github ssh key to their agent and it will be available inside the VM.
* `dvm`, `omnibus`, `erchef` [520](https://github.com/chef/chef-server/pull/520) - direct\_solr\_writes
  * add support for immediate data commits to `chef_solr`, bypassing the rabbit queue and expander process.
    Enable this by setting `opscode_erchef['search_queue_mode']` to `batch`.
* `oc-id` [522](https://github.com/chef/chef-server/pull/522) - CVE-2015-1840
  * Upgrade `jquery-rails` to patch CVE-2015-1840
* `bookshelf`, `chef-mover`, `bifrost`, `erchef` [518](https://github.com/chef/chef-server/pull/518) - rebar-update-pc-fix
  * An update to the port compiler on hex caused an incompatibility with the
    version of rebar we had vendored. Here we lock the pc plugin to avoid
    the problem.
    We should try to move to a newer rebar3 and update the port compiler
    once rebar3 does a release.
* `chef-mover`, `bifrost`, `erchef` [507](https://github.com/chef/chef-server/pull/507) - ok-rebar-you-win
  * rebar3 wants to alphabetize the rebar.lock file, who are we to argue?
* `chef-mover` [508](https://github.com/chef/chef-server/pull/508) - ignore-ance-is-bliss
  * Remove `oc_erchef` build artifacts from git
* `omnibus` [509](https://github.com/chef/chef-server/pull/509) - sles-support-csc-install
  * Adding suse to package support for local addon installs.

## 12.2.0 (2015-09-01)

### oc\_erchef
* New policyfile API endpoints to enable cleanup of policy objects:
  * `/policies/:policy_name` (GET, DELETE)
  * `/policies/:policy_name/revisions` (POST)
  * `/policies/:policy_name/revisions/:revision_id` (GET, DELETE)
  * `/policy_groups/:policy_group_name` (GET, DELETE)
* admin group acl policy changes, preventing removal of admin group ACE
  from a group's grant ACL.
* renamed `$ORG_global_admins` to `$ORG_read_access_group`
* prefer user auth when there is a username/client collision and the
  request is originating from Manage.


### omnibus
* Change oc-id vip back to 127.0.0.1 to avoid possible
  error with nginx; add -b option for Rails and make vip fully
  configurable so it can work properly in IPv4 and IPv6 environments
* Ensure automatic updates from the chef packagecloud repository
  are disabled on rhel by default, and in all cases specify stable
  repository.
* Ensure that `opscode_chef` database is owned by the `sql_user` specified for
  `opsode-erchef` instead of the global postgresql user.
* external postgresql now supported
* change nearly all database access (except initial DB creation for
  locally managed database) to use tcp/ip instead of local socket for
  consistency in local/remote installations.
* add-on configuration hook framework
* chef-server-ctl support for pre/post command hooks via omnibus-ctl
* chef-server-ctl support for external postgresql
* new chef-server-ctl commands: psql, backup, restore
* chef-server-ctl will give a nice message instead of a stack trace when
  not run as root.

### dvm
* new option to auto-load components that live in omnibus prior to first
  chef-server-ctl reconfigure
* support and auto config for an additional postgres VM.

### bifrost
* fix for deadlocks that occur when multiple updates to the same actor are applied
  concurrently.

### oc-id
* additional fix for not enabling newrelic unless requested

### chef-mover
* New migration for the rename of `$ORG_global_admins` to
  `$ORG_read_access_groups` and proper setup of org user read
  permissions.

### bookshelf
* Experimental support for synchronizing two bookshelf instances.

### Components

New Components
* `chef_backup-gem` (0.0.1.dev.4)

Updated Components
* `omnibus-ctl` (c514d1d4 -> 0.4.1)
* `knife-opc` (17d4fc26 -> 528be923)
* `knife-ec-backup` (2.0.4 -> 2.0.6)
* `ohai` (2accf7e2 -> ffd9a0a0)
* `chef` (9a3e6e04 -> 8926514f)

## 12.1.2 (2015-07-16)

### chef-server
* Fix issue where chef-server-ctl install could not fetch remote packages via apt.

## 12.1.1 (2015-07-13)

### chef-server
* Fix problems with upgrades from Open Source Chef Server 11 related
  to client and user uploads.
* Fix problems with upgrades from Enterprise Chef Server 11 related to a failed chef-mover migration.
* Upgrade to openssl 1.0.1p

* Upgrade to libxml 2.9.2

### Components

### knife-ec-backup
* Version 2.0.4 pulled in to fix Open Source Chef Server 11 upgrade bugs related to API versioning.

## 12.1.0 (2015-06-19)

### chef-server
* new self-contained development environment for chef server
* Remove nested directories from log rotation template
* Fix local-mode-cache warnings on `chef-server-ctl reconfigure`:
  Move the `cache_path` into /var/opt/opscode to avoid warnings
* Float Chef and knife-opc on master
* update chef-sever-ctl key commands to use Chef::Key.
* Correct path to DRBD split brain notification script.
* remove SquareSerif font, comm-503 page, and associated resources
* Server's install of Chef now floats on master.
* Server's install of knife-opc now floats on master.
* Remove install message from postinst package script
* Update chef-server-ctl key commands to use chef-client's Chef::Key object.
* New gather-log script gathers a lot more debugging information.
* removed unused error json and html pages. Correct doctype in default
  landing page.
* Ensure that postgres shared buffers are calculated correctly
* Adding support for being able to use external rabbitmq box for data to
  be sent into analytics. This would mean that miltiple chef-servers
  could send info into one analytics via external rabbitmq.

### Components

### oc_erchef
* API v1 now available
* API v0 deprecated
* Update policyfile URLs to match draft RFC
* significant performance improvements
  * create `bulk_fetch_query` to replace multiple repeated db calls,
    return less data, and reference fewer tables.
  * sqerl updates to reduce unncessary requests, and batch all steps of
    a binding and executing a query into a single call to postgres.
* X-Ops-Server-API-Info response header now implemented for all API
  versions
* [refactor] reduce copy-paste of core functionality by allowing
  callbacks for response body customization on create/update.
* dialyze everything - clean dialyzer build
* enable `warnings_as_errors` build flag
* consistency in sql statement loading across `chef_db` and
  `oc_chef_authz_db`.
* add support for server-side generation of keys via the keys API by
  accepting `create_key`: true in the request body.
* [refactor] consolidated key manipulation and validation to one place
  in the code.
* Cookbook Artifacts API interops w/ chef-client and ChefDK
* Pull in newest folsom and bear to address folsome_graphite crashes.
* Thanks to @danieldreier for removing satan from the development guide
* Redact password from actions data, if present.

### oc-id
* interationalization and other improvements to password change
* don't call home to newrelic unless specifically configured with a
  newrelic API key
* fix asset precompile:
  * Use HTTPS rather than git URLs for gems from GitHub
  * Update to latest web core
  * Add assets precompile to Travis CI step
  * Changes to remove deprecation warnings
* new profile controller and views
* updated UI using chef-web-core

### oc-chef-pedant
  * Versioned testing support for users, clients, principals,and
    response headers.
  * Bring artifacts & policyfile test into line with final implementation
  * exposed 'server\_api\_version to tests.


#### Merged Repositories

12.1.0 is the first release using the new merged repository which
contains the following components:

- opscode-omnibus
- oc\_erchef
- oc\_bifrost
- oc-id
- bookshelf
- opscode-expander
- chef-mover
- chef-server-bootstrap

#### Updated Components

* zlib (1.2.6 -> 1.2.8)
* libffi (3.0.13 -> 3.2.1)
* omnibus-ctl (89423eda -> c514d1d4)
* postgresql92 (9.2.9 -> 9.2.10)
* server-jre (7u25 -> 8u31)
* knife-opc (7bf26f4b -> daec05e7)
* python (2.7.5 -> 2.7.9)
* opscode-solr4 (4.5.1 -> 4.9.1)
* chef (12.0.3 -> 4664b73)

### opscode-omnibus

### redis 2.8.21

* Multiple bug fixes since 2.8.21:
https://raw.githubusercontent.com/antirez/redis/2.8/00-RELEASENOTES
* CVE-2015-4335: Redis Lua Sandbox Escape

### postgresql 9.2.10
* bugfixes: [link](http://www.postgresql.org/docs/9.2/static/release-9-2-10.html)
* CVE-2015-0241: Fix buffer overruns in `to_char`()
* CVE-2015-0242: Fix buffer overrun in replacement `*printf()` functions
* CVE-2015-0243: Fix buffer overruns in `contrib/pgcrypto`
* CVE-2015-0244: Fix possible loss of frontend/backend protocol synchronization after an error
* CVE-2014-8161: Fix information leak via constraint-violation error messages
* CVE-2014-0067: Lock down regression testing's temporary installations on Windows
-----------------------------
## 12.0.8 (2015-04-20)

### chef-server-ctl
* Added rspec testing basics for chef-server-ctl commands
* Updated and added testing for key rotation related chef-server-ctl commands

### oc\_erchef 1.7.0
* introduces server api versioning per chef-rfc/rfc-041.  As of 1.7.0
  the only supported version is 0.
* significant internal refactoring and cleanup

### oc-chef-pedant 2.0.5
* tests for server api versioning, and by default pass
  x-ops-server-api-version to the server on all requests.

### opscode-omnibus
* use keys API for key rotation in chef-server-ctl, instead of direct
  database access.
* lua routing tests working again
* travis support enabled
* centos-7/rhel-7 enabled for local builds

### chef-mover
* now floating on master

## 12.0.7 (2015-03-26)


### oc\_erchef 1.6.4
* Policyfile endpoint URLs updated to match Chef RFC 042
* Cookbook Artifacts endpoint for policyfiles
* Miscelaneous build improvements

### oc\_erchef 1.6.3
* Search results respect ACLs.

## 12.0.6 (2015-03-19)

### opscode-omnibus

* Use a cert instead of a public key for pivotal.
* No longer generate /etc/opscode/pivotal.cert as it is no longer used.
* Remove the public key we now use for bootstrapping (/etc/opscode/pivotal.pub) post bootstrap so that it only lives in the database.
* Disable jmxremote in solr4's Java options
* Configuration options for the key cache are now exposed in /etc/opscode/chef-server.rb

### knife-opc 0.3.0

* Ensure keyfile is writable before creating a user.
* Add --input option to user-edit command
* Add user to billing-admins group with --admin is passed
* Print new private-key when user-edit results in a key generation

### bookshelf 1.1.7
* Uses relx for Erlang application releases
* Upgraded to lager 2.1.1

### oc\_bifrost 1.4.6
* Uses relx for Erlang application releases
* Upgraded to lager 2.1.1

### oc\_erchef 1.6.2
* Uses relx for Erlang application releases
* Upgraded to lager 2.1.1

### chef-server-bootstrap 1.0.1
* Updated to use public key instead of certificate for pivotal on bootstrap.

### oc-chef-pedant 2.0.1
* Adds tests for keys named get
* Integrates chef-pedant into oc-chef-pedant.

### oc-chef-pedant 2.0.3
* Adds tests for keys named delete and put

### oc\_erchef 1.6.5
* Support to GET, PUT, and DELETE a named key

### oc\_erchef 1.6.1
* Integrates schema into oc\_erchef itself
* Adds policyfile validation support
* License and readme updates

### openssl 1.0.1m
* CVE-2015-0286: Segmentation fault in ASN1_TYPE_cmp fix
* CVE-2015-0287: ASN.1 structure reuse memory corruption fix
* CVE-2015-0289: PKCS7 NULL pointer dereferences fix
* CVE-2015-0293: DoS via reachable assert in SSLv2 servers fix
* CVE-2015-0209: Use After Free following d2i_ECPrivatekey error fix
* CVE-2015-0288: X509_to_X509_REQ NULL pointer deref fix

## 12.0.5 (2014-02-26)

### bookshelf 1.1.6
* Updated to webmachine 1.10.8

### oc\_bifrost 1.4.5
* Updated to webmachine 1.10.8

### oc-chef-pedant 1.0.79
* New keys API tests
* New cookbook artifact API tests

### oc\_erchef 1.5.0
* Keys API POST support: /organizations/$ORG/clients/$CLIENT/keys and
  /users/$USER/keys

### oc\_erchef 1.4.2
* the fields `external_authentication_uid` and `recovery_auth_enabled`
  are now preserved on user PUT when they are not provided.

### oc\_erchef 1.4.1
* New GET/POST `BASE_URL/cookbook_artifacts/NAME/IDENTIFIER` endpoint
* Updated to webmachine 1.10.8

## 12.0.4 (2014-02-19)

### opscode-omnibus
* nginx bookshelf caching, enabled with
  `opscode_erchef['nginx_bookshelf_caching'] = :on`
* s3 URL expiry window setting,
  `opscode_erchef['s3_url_expiry_window_size']`, which can have values
  in minutes (e.g. `"15m"`), percentage (e.g. `"15%"`), or just be
  `:off`.
* Ensure shell metacharacters in arguments to chef-server-ctl user-
  and org- commands are properly handled.
* Pull in chef-client 12.0.3.
* Update rabbitmq cookbook to be compatible with modern chef-client.
* Update pivotal and knife-ec-backup knife configs to be compatible with modern chef-client.
* Use chef-client -z instead of chef-solo in the server.

### oc\_erchef 1.4.0
* keys API: new GET support for `/users/$user/keys` and `/organizations/$org/clients/$client/keys`
* module epgsql brought up to current.
* Fix LDAP regressions related to multiple fields, anonymous bind, and group\_dn

### oc\_erchef 1.3.1
* Add incubation feature for policyfiles. Feature flag off by default.

### oc\_erchef 1.2.2
* Add `s3_url_expiry_window_size` setting for s3 URL caching.

### oc-chef-pedant 1.0.76
* test support for keys API endpoint (GET)

### oc-chef-pedant 1.0.75
* test support for policyfile endpoints

### omnibus-ctl 0.3.2
* Use chef-client -z instead of chef-solo.
* Reference chef-client via `base_path`.

### knife-ec-backup 2.0.1
* Added keys table / key rotation support.

### ruby 2.1.4
* Needed for ohai >= 2.

### chef-gem 12.0.3
* [chef-client 12 changelog](https://docs.chef.io/release_notes.html#what-s-new).

## 12.0.3 (2015-02-04)

### enterprise-chef-common 0.5.1
* Add preliminary systemd support

### enterprise-chef-common 0.5.0
* Make it possible to pass arbitrary attrs to runit resources

### chef-pedant and oc-chef-pedant
* Updated chef-pedant to 1.0.41, oc-chef-pedant to 1.0.73. These
  versions have been updated to use RSpec 3.

### opscode-omnibus
* Added key management and rotation commands add-client-key,
  add-user-key, delete-user-key, delete-client-key, list-client-keys,
  and list-user-keys.
* Pulled in Chef 11.18.0. This will fix "ffi-yajl and yajl-ruby gems
  have incompatible C libyajl libs" warning when running
  chef-server-ctl commands.
* Ensure nginx restarts on frontends after lua-related changes
* Updated nginx's logrotate config with proper log ownership.
* Nginx logs $http_x_forwarded_for instead of $remote_addr if
  nginx['log_x_forwarded_for'] is true. The default is false
* Log an error and exit when DRBD mount attempts are
  exhausted rather than entering an infinite loop.
* Fix installation errors caused by PERL5LIB environment
  variable
* chef-server-ctl now returns non-zero exit codes for errors
  during user and organization-related commands.
* Use -D for --download-only option in
  chef12-upgrade-download command, avoiding option name conflict.


### oc\_erchef 1.2.0
* add basic multikey/key rotation support. This is not yet exposed via
  the REST API, but is being used within `oc_erchef` itself.

### oc\_erchef 1.1.1
* Updated `sqerl` version to pull in more current `epgsql` dependency
* Pulled repos `chef_db`, `chef_index`, `chef_objects`, `depsolver`,
  `oc_chef_authz`, and `oc_chef_wm` into apps in `oc_erchef`.
* Pulled `chef_wm` into `oc_chef_wm`.
* Updated integration tests, and got integration and unit tests
  running in Travis CI.
* Remove array merging in `chef_deep_merge`, fixing incorrect search
  results for arrays.

### opscode-chef-mover 2.2.19
* Updated mover to pull in oc\_erchef since some dependencies where moved there.

### enterprise-chef-server-schema 2.4.1
* Use HTTPS instead of GIT to pull down dependencies in Makefile.

### opscode-omnibus
* merged `oc_erchef` configuration sections for `chef_wm` into `oc_chef_wm`

## 12.0.2 (2015-01-27)

### chef-mover 2.2.20
* Fix bug that can cause long-running migrations to hang indefinitely

### private-chef-cookbooks
* Expose configurable value for database bulk fetch batch size to
  use during Solr 4 migrations

## 12.0.1 (2014-12-17)

### oc-id
* Update to version 0.4.4 to patch a doorkeeper CSRF vulnerability

### chef-mover
* update to version 2.2.17, with better failure case handling and
  increased timeouts.

### oc-chef-pedant 1.0.68
* pin mixlib-shellout to 1.6.1

### opscode-omnibus
* pin mixlib-shellout to 1.6.1
* added new `group_dn` ldap attribute to require users to be in the
  named group.
* Refactored superuser bootstrap process to use new chef-server-bootstrap
  repository instead of opscode-test, which pulled in a variety of now
  deprecated ruby repositories.
* Update location/name of Chef’s public GPG key.
* Fetch chef-server-ctl man page directly from chef-docs repo.

### chef-server-bootstrap 1.0.0
* Repository that replaces opscode-test, allowing us to deprecate several
  old ruby repositories.

### oc\_erchef 0.30.0
* module `chef_wm` merged into `oc_chef_wm`
* support for ldap user search including memberOf group,
  via attribute `group_dn`

## 12.0.0 (2014-11-25)

### enterprise-chef-common 0.4.7
* Restart logging service on log configuration change

### enterprise-chef-common 0.4.6
* Make project-ctl configurable by name

### omnibus-ctl 0.3.1
* Exclude gz files from tail

### private-chef-cookbooks
* Add `ip_mode` and `normalize_host` for ipv6 configuration
* Add configuration for queueing in pooler
* Expose `db_timeout` for sqerl in Erchef, bifrost and mover as a parameter
  that can be set in the "/etc/opscode/chef-server.rb" file for convenience.
  By default there is a hard coded value of 5 seconds (5000ms) as per:
  [sqerl\_client.erl](https://github.com/opscode/sqerl/blob/master/src/sqerl_client.erl#L134)
* Select appropriate default port for LDAP and LDAPS (when encryption is
  selected, as previously user had to manually add port to make it work).
* Expose `proxy_connect_timeout` for Nginx when it connects to the backends,
  so it can be adjused. The hard coded default might not be sufficient in
  some cases.
* Expose `folsom_graphite` configuration, default to disable
* Move Postgres database stop/start out of migrations
* Gracefullly attempt to start the database during migrations

### opscode-omnibus
* Add ability to configure SQL query timeout for Erchef, bifrost and mover.
* Provide reasonable default for LDAP and LDAPS ports.
* Deprecate ldap "encryption" setting and replace with
  `ssl_enabled`/`tls_enabled`. Add further validation and sanity checks around
  ldap settings, as well as deprecation warnings.
* Add ability to configure timeout for connect() when connecting to backends.

### oc\_erchef 0.29.4
* fix issue in which local mode auth was not handled correctly,
  preventing accounts on an LDAP server from being associated
  with existing Chef Server accounts when the login name differed.

### oc-chef-pedant 1.0.67
* Modify test of local mode authentication to be correct

### oc-chef-pedant 1.0.66
* Turn org creation validation off by default

## 12.0.0.rc6 (2014-11-11)

### oc-chef-pedant 1.0.65
* Add test for /organizations/:org\_id/ANY/\_acl endpoint

### oc-chef-pedant 1.0.64
* Add coverage for /users/USER/organizations endpoint

### oc-chef-pedant 1.0.63
* additional test for proper behavior when attempting to remove an org's
  admin.
* Update tests to reflect that clients no longer have C/U/D permissions
  on data bags by default.

### oc-chef-pedant 1.0.62
* Fix for consistent return values in oc\_erchef

### oc\_erchef 0.29.3
* route /organizations/:org\_id/ANY/\_acl endpoint

### oc\_erchef 0.29.2
* set default client ACLs for data bags to read-only.  See Release Notes for i
  important related details.
* correct message logging in org-user association/disassociation process
* new /controls endpoint in support of upcoming client features

### oc\_erchef 0.29.1
* revert functionality change where erchef version of /users/X/organizations
  endpoint no longer returned "guid" field. This field is used by internal
  products  in our hosted environment and cannot yet be removed.
* fix regression in which organization user was partially removed
  even though removal was disallowed because user is an admin.
* update actions to support capture of acl activity

### oc\_erchef 0.29.0
* Internal placeholder we used to indicate our *hosted* product
  switch from Erlang R15B03-1 to R16B03-1.  Note that R16B03-1 has been
  included in CS12 since the first RC.

### oc\_erchef 0.28.5
* update sqerl to use queuing-enabled pooler API
* update pooler to 1.3.3, which adds queueing support

### oc\_erchef 0.28.4
* Add folsom-graphite dependency (used for runtime stats gathering)

### oc\_erchef 0.28.3
* fix regression that broke org caching
* Org support in postgres
* Reindexing support to check redis flags
* Fix typo in darklaunch interrogation

### oc\_id
* Set `VERSION` environment variable on database migrations to avoid conflict
  during upgrades

### opscode-omnibus
* changes to addon installs to default to lucid when current ubuntu codename isn't in the accepted list (to support installs on 14)
* added apt-transport-https package in case it was missing from the system (packagecloud requires it)
* created chef-server.rb during install to cut down on user confusion
* [opscode-omnibus-597] Limit postgresql shared memory usage to stay under SHMAX
* Change postgres effective\_cache\_size to 50% of available RAM instead of hard coding at 128MB
* updated references to omnibus-ruby repo to be omnibus
* changelog - fix markdown formatting errors
* changelog - added this changelog note

### private-chef-cookbooks
* [OC-11769] make oc\_chef\_authz a tunable in private-chef.rb
* Fix oc\_chef\_authz timeout tunable
* Make postgresql slow query logging configurable
* Fix missing resources on API HTML pages
* Fixed the default value for Postgres effective\_cache\_size
* Adjust perms to 0750 for all service's log dir
* Add and use new perms attribute
* Add an OmnibusHelper method to provide an owner and group hash

### chef-server-ctl
* Partition server start/stop in upgrade process
* Changed commands org-associate and org-dissociate to org-user-add and org-user-remove, respectively.
* Update password command to use knife-opc so as to work post-removal of mixlib-authorization.

## 12.0.0.rc5 (2014-10-17)

### openssl - 1.0.1j
- SRTP Memory Leak (CVE-2014-3513)
- Session Ticket Memory Leak (CVE-2014-3567)
- Build option no-ssl3 is incomplete (CVE-2014-3568)

### opscode-omnibus
* properly configure ldap under erchef, and add some safeguards
  against incorrect encryption configuration.
* oc\_erchef updated to 0.27.4
* Bump the chef\_max\_version to 12 (this is the max chef client version that Chef Server will accept)
* expose license configuration options
* Add man page for chef-server-ctl.
* Correct gather-logs to point to chef-server.rb
* Disable SSLv3 support in nginx
* Added command line options to open-source-to-chef-server-12 upgrade for finer-grained control of migration process

### oc\_erchef 0.27.7
* Improve error handling in org creation and deletion.

### oc\_erchef 0.27.6
* Fixed pooler bug with regard to timed out pool member starts

### oc\_erchef 0.27.5
* Add org info to actions

### oc\_erchef 0.27.4
* ldap start\_tls support
* ldap simple\_tls support
* support for correctly looking up users by external auth id
* fix for GET of org users not returning correct state record, resulting
  in requests not properly terminating

### oc\_erchef 0.27.3
* Fix meck dependency locking issue.

### oc\_id 0.4.2
* Add support for Chef signed headers in Resource Owner Password
  Credentials flow
* Add new endpoint (/v1/me/organizations) to get the list of
  organizations for the user represented by a Bearer token
* Update doorkeeper gem to 1.4.0
* Add support for Resource Owner Password Credentials flow

### opscode-chef-mover 2.2.15
* Clean up error handling for org user associations and invites migrations
* Fix backwards compatibility issues with oc\_chef\_authz intergration

### rest server API
* removed check for maximum client version (only checks for minimum, i.e., <10)
* updated server flavor from 'ec' to 'cs' (Chef Server) now that servers have been merged

### chef-server-ctl
* Restricted chef-server-ctl install to known Chef packages
* Correct show-config command/recipe to point at chef-server.rb instead of private-chef.rb
* Updated knife-opc config so that user / org / association commands now work if non-default ports are used.
* re-enable ctrl+c for chef-server-ctl commands by setting "client\_fork false" in solo.rb

### omnibus-ctl 0.3.0

* Extended API with `add_command_under_category`, that allows ctl projects to group commands under categories, resulting in more logical help output.
* Added concept of hidden services that hides certain services from those listed in `chef-server-ctl status`.
* Any service (even hidden ones) can still be status checked via `chef-server-ctl status <service>`.
* opscode-chef-mover was added as a hidden service.

### oc-chef-pedant 1.0.60
* add support for ssl version configuration

### oc-chef-pedant 1.0.59
* Fix rspec deprecations
* Remove test of curl

## 12.0.0.rc4 (2014-09-17)

### opscode-omnibus
* Ensure contents of install dir (`/opt/opscode`) are owned by root.
* Configure oc-chef-pedant ssl version to match nginx

## 12.0.0

### Renamed chef server core instead of Private Chef or Enterprise Chef.

### opscode-omnibus
* Change to using /etc/opscode/chef-server.rb from /etc/opscode/private-chef.rb
* Symlink private-chef.rb to chef-server.rb if private-chef.rb is present

### bookshelf 1.1.4
* Erlang R16 support

### cacerts 2014/08/20
* Update to latest cacerts as of 2014/08/20

### chef-ha-plugin
* Add support for pluggable high availability system

### chef-sql-schema removed
* We use a sqitch based schema instead.

### couchdb removed
* We are pleased to announce that we have migrated all data over to sql.

### enterprise-chef-server-schema 2.4.0
* Updates org\_migration\_state table with migration\_type and verification
* Update org\_migration\_state with support for solr 4 migration
* Cleans up reporting schema info table
* Clean up Makefile to preserve PATH variable
* Update password hash type for OSC password hash types
* Fix constraints for org\_user\_assocations and org\_user\_invites
* Add tables for organizations, org\_user\_associations, and org\_user\_invites

### erlang R16B03 added
* Replaced R15, which was only used by the services we removed.

### knife-ec-backup
* Add support for tools to backup and restore from chef servers.

### oc-chef-pedant 1.0.57
* Remove /system-recovery endpoint tests
* Enhance test coverage for user-org association
* Update acl, organization and association tests for ruby-erlang differences
* Add tests for
  * authenticate\_user endpoint
  * users email validation
  * superuser access
  * certs in pubkey field for user
  * default organization rewriting
  * verify-password

### oc\_authz\_migrator removed
* oc\_authz\_migrator is no longer needed

### oc\_erchef updated to 0.27.3

#### oc\_erchef 0.27.3
* Organizations in erchef and in sql
* organization association and invites in erchef and sql

#### oc\_erchef 0.26
* Initial low level work for organizations and associations in SQL
* Improve reindexing script
* ACL endpoint in erchef
* Add chef action data\_payloads

#### oc\_erchef 0.25
* Add default organization support for OSC compatibility
* Add license endpoint support
* Add global placeholder org macro.
* System recovery endpoint work: Fix so recovery\_authentication\_enabled is correct for new users
* Add internal chef keygen cache to replace opscode-certificate service.
* do not force user key type to public on regeneration
* Bugfix for concurrent cookbook uploads
* Automatically upgrade user password salt algorithm on auth
* Cleanups for user password encryption
* Groups endpoing in sql and in erchef
* Update authenticate\_endpoint for LDAP
* Update chef users email validation and filtering
* Add chef users endpoint.

### opscode-account removed
* The last remaining endpoints (organizations, and user-org
  association and invites) are entirely implemented in erchef now.

### opscode-certificate removed
* This is replaced by the keygen service in erchef.

### opscode-chef-mover 2.2.14
* Organizations, user-org association, and user-org invite migrations from couchdb to SQL
* Migration of global containers and global groups from couchdb to SQL
* Backwards incompatible API change: Group creation (POST) ignores users and clients
* Containers and groups migration from couchDB to postgreSQL
* Bcrypt user migrations
* Solr4 migration
* Generalized migrate scripts and other code to be migration\_type agnostic
* Improved support for non-org based migrations
* Update for Erlang R16

### opscode-org-creator removed
* Erchef no longer needs multi-phase organization create; direct creation is sufficient.

### opscode-platform-debug and orgmapper removed
* Orgmapper is no longer useful after migrations to SQL are complete.

### Replace solr 1.4 with solr 4
* Upgrade to solr 4.

### Remove opscode-webui.
* It is superceded by the opcsode-manage package

### postgresql 9.1 removed

### private-chef-administration
* Removed. Docs can be found at docs.chef.io

### private-chef-cookbooks
* Introduce pluggable HA architecture as an alternative to DRBD
* [OC-10117] opscode-solr4 accepts Java-like memory attributes
* [OC-11669] keepalived safe mode

### ruby updated to 1.9.3-p547
* Update is from 1.9.3-p484

### unicorn removed
* No longer needed because opscode-account is gone

### chef-server-ctl
* Renamed from private-chef-ctl
* Added chef-server-ctl upgrade command to support migrations from the open source chef 11 server
* Added tooling to manage users and orgs from the command line via knife-opc
* Added chef-server-ctl install command to install chef add-on packages (via web or local file)
* Clarify the use of the --path options for the `install` subcommand

### omnibus-ctl
* [OC-10470] Allow private-chef-ctl status to ignore disabled services.
* [OC-11574] private-chef-ctl service commands should be HA aware
* [OC-9877] exclude binary files and archives from \*-ctl tail

## 11.2.2 (2014-09-17)

### opscode-omnibus
* Ensure contents of install dir (`/opt/opscode`) are owned by root.

## 11.2.1 (2014-08-29)
### enterprise-chef-common
* Update to 0.4.5
* Fix issue where 'private-chef' was being changed to 'private\_chef' unexectedly in upstart/runit files

## 11.2.0 (2014-08-29)

### Makefile
* Add Makefile for automating builds

### adding actions\_payload 2014.08.15
* [CA-555] Update 11.1-stable oc\_erchef with latest oc\_chef\_action

### postgresql 2014.07.29
* [OC-11672] Upgrade PostgreSQL to 9.2.9

### enterprise-chef-common 2014.07.21
* [OC-11575] Don't start services by default in HA topology
* Update to 0.4.4

### oc\_chef\_actions 2014.07.03
* Update to latest of oc\_chef\_action to get hostname from fqdn instead
  of inet
* Setting the CHEF\_ACTIONS\_MESSAGE\_VERSION to 0.1.0
* Sets ['dark\_launch']['actions'] = true

### cacerts 2014.04.22
* Update to latest cacerts as of 2014-04-22

### chef 11.12.2
* Update embedded chef gem to 11.12.2

### opscode-platform-debug rel-0.5.1
* Add authz API support

### opscode-software
* Refactor PERL Postgres driver installation

### private-chef-cookbooks
* [analytics] Copy webui\_priv into opscode-analytics if actions is enabled
* [OC-11297] Tweak partybus migration-level subscribes for a more reliable
  workaround
* [OC-11459] Allow opscode-manage to easily be moved off of 443
* [OC-11540] Fix invalid opscode-account config when forcing SSL
* [OC-11601] Fix a race condition that sometimes caused redis\_lb to attempt to
  reconfigure itself before it was restarted.
* [OC-11668] Enable ipv6 in standalone mode
* [OC-11673] Tune PostgreSQL keepalive timeouts
* [OC-11710] Fix couchdb compaction log rotation
* Add bifrost\_sql\_database uri to orgmapper.conf
* [OC-11585] Allow ['lb']['upstream'] to have a custom setting
* [CHEF-3045] increase s3\_url\_ttl from 15m to 8h
* Use SSL port for lb\_internal if non-SSL is disabled
* Lock down postgresql

### private-chef-ctl

* Add a gather-logs command to create a tarball of important logs and
  system information for Chef Support
* [OC-9877] Fix bug that included binary files and archives when using
  'private-chef-ctl tail'

### oc-id 0.3.3
* Add Chef Identity Service (oc-id)

### openssl 1.0.1i
* Fix for CVE-2014-3512
* Fix for CVE-2014-3511
* Fix for CVE-2014-3510
* Fix for CVE-2014-3507
* Fix for CVE-2014-3506
* Fix for CVE-2014-3505
* Fix for CVE-2014-3509
* Fix for CVE-2014-5139
* Fix for CVE-2014-3508

### rabbitmq 3.3.4
* Upgrade to RabbitMQ 3.3.4

### opscode-account rel-1.51.0
* [OC-11702] - fails to expand ACLs and groups when they contain
  groups that no longer exist
* [OC-11708] - fixes user association bug that relied on permissions
  of the last updater of the users group

## 11.1.8 (2014-06-26)

### oc\_authz\_migrator 0.0.2
* exit immediately on errors


## 11.1.7

### private-chef-cookbooks
* [OC-11499] Use more strict regular expression for IP check in ha-status
* [OC-3107] Ensure CouchDB compaction cron job does not run on passive
  backend.
* [OC-11601] Restart redis\_lb immediately during reconfigure
* [OC-11490] Explicitly set keepalived directory ownership
* [OC-11297] EC 11 fresh install not saving migration state
* [OC-11656] Set explicit owner and group for services without them
* Address a PostgreSQL configuration error. The defect allows any local user on the system hosting the Chef Server’s PostgreSQL components full access to databases.
* [OC-11662] Separate redis\_keepalive\_timeout from redis\_connection\_timeout and increase their default values from 60ms to 1000 and 2000ms, respectively.

### private-chef-ctl
* [OC-11657] Bump default svwait timeout of 7 seconds to 30 seconds
* [OC-11382] keepalived restart interferes with upgrades
* [OC-8881] private-chef-ctl password does not work

### configurable postgresql unix user
* Update gather-logs and migration scripts to honor postsgresql['username']

## 11.1.6 (2014-06-05)

### openssl 1.0.1h
* Address vulnerabilities CVE-2014-0224, CVE-2014-0221, CVE-2014-0195,
  CVE-2014-3470 https://www.openssl.org/news/secadv\_20140605.txt
  return code

### private-chef-cookbooks
* [OC-11581] private-chef-ctl test command should return the pedant
  return code

## 11.1.5 (2014-05-14)

### oc\_erchef 0.24.6
* rename oc\_actionlog to actions

### private-chef-cookbooks
* Use dark launch to enable Chef Actions (default: off)
* Write out Actions configuration file for use by opscode-analytics

## 11.1.4 (2014-05-07)

### oc-chef-pedant 1.0.29
* Add tests for superuser password authentication

### opscode-account rel-1.49.0
* Prevent password authentication for pivotal superuser

### opscode-platform-debug rel-0.4.6
* Remove legacy chargify code
* Updated knifetests to work with the latest reporting API

### private-chef-cookbooks
* platform\_family fixes to couchdb and drbd cookbooks
* Set random initial password for pivotal user on bootstrap

## 11.1.3 (2014-04-09)

### berkshelf
* new dep: libffi
* new dep: libarchive

### curl 7.36.0
* CVE-2014-0138: libcurl can in some circumstances re-use the wrong connection when asked to do transfers using other protocols than HTTP and FTP
* CVE-2014-0139: libcurl incorrectly validates wildcard SSL certificates containing literal IP addresses when built to use OpenSSL
* CVE-2014-1263: When asked to do a TLS connection (HTTPS, FTPS, IMAPS, etc) to a URL specified with an IP address instead of a name, libcurl built to use Darwinssl would wrongly not verify the server's name in the certificate
* CVE-2014-2522: When asked to do a TLS connection (HTTPS, FTPS, IMAPS, etc) to a URL specified with an IP address instead of a name, libcurl built to use Winssl would wrongly not verify the server's name in the certificate

### chef
* upgrade to version 11.10.4

### erlang
* upgrade to r15b03-1

### nokigiri
* upgrade to nokigiri 1.6.1

### libyaml 0.1.6
* CVE-2014-2525: Heap-based buffer overflow allows context-dependent attackers to execute arbitrary code

### oc\_erchef 0.24.2
* add oc\_chef\_action to oc_erchef (support for opscode-analytics actions package)

### openssl 1.0.1g
* CVE-2014-0160: heartbeat extension allows remote attackers to obtain sensitive information from process memory

### opscode-account 1.48.0
* fix USAG and organization creation for sql
* fix bug where billing-admins creation crashed for sql
* gracefully fail association request if org is in 504 mode
* speed up internal org-creation by removing Couchdb \_all\_dbs call
* check org \_route endpoint for groups darklaunch during org creation
* fix schema constraint bug during LDAP user creation

### opscode-webui 3.8.13
* Ruby on Rails security updates

### postgresql
* upgrade to 9.2.8

### private-chef-cookbooks
* Increase postgresql max\_connections to 350 to handle 4 node cluster
* Manage permissions for /var/log/opscode for non 0022 umasks

### private-chef-ctl
* Remove incorrect mention of `heartbeat_device` from `ha-status` output.

### chef-pedant 1.0.27
* added CLI options for running /internal-organization endpoint tests
* added tag for running organization tests
* add association tests to tags list

### oc-chef-pedant 1.0.28
* added test coverage for /organization and /internal-organization endpoints
* added association framework and tests

## 11.1.2 (2014-02-28)

### posgresql
* Add ossp-uuid extension to Postgres 9.2

### libossp-uuid 1.6.3
* Add libossp-uuid library for Postgres

### private-chef-cookbooks
* Configure oc\_actionlog in oc\_erchef and rabbit
* Remove :session and :environment from webui exception emails
* Add internal /\_routes endpoint to load balancer

## 11.1.1 (2014-02-17)

### private-chef-cookbooks

#### BUGFIXES
* remove banned/whitelist IP checking from OpenResty Lua config that breaks ipv6 clients

## 11.1.0 (2014-02-06)

### omnibus-ruby 1.3.0
* https://github.com/opscode/omnibus-ruby/blob/master/CHANGELOG.md#130-december-6-2013

### omnibus-software 3d9d097332199fdafc3237c0ec11fcd784c11b4d
* [keepalived] update to 1.2.9 + patch for Centos 5.5
* [perl] generate an Omnibus-friendly CPAN config
* [openssl] CVE-2013-4353/CHEF-4939 - tls handshake causes null pointer in OpenSSL
* [berkshelf] update to 2.0.12
* [libyaml] CVE-2013-6393 - update libyaml to 0.1.5

### redis-rb 3.0.6
* Add redis gem for reconfigure management of redis install

### openresty-lpeg 0.12
* Add Lua lpeg library for use in refactored openresty routing config

### redis 2.8.2
* Add back in for use in openresty routing config

### bookshelf 1.1.3
* Remove request logging, which causes backups and crashing under heavy load

### enterprise-chef-server-schema 2.2.3
* Add containers table
* Add new enum type and columns for user password hash
* Add groups table
* Add index for opc\_users(customer\_id) (improves delete performance)

### oc-chef-pedant 1.0.25
* [CHEF-4086] Add tests for cookbook version host header changes
* Add tests to validate newly created organizations
* Updates to /containers endpoint tests for ruby / erlang switching
* Updates to /groups endpoint tests for ruby / erlang switching
* Use IPV6-compatible rest-client gem for testing IPV6
* Add tests for /users/:user/\_acl endpoint
* Update /principals endpoint tests for pushy updates

### oc\_bifrost 1.4.4
* Add IPV6 support
* Use shared opscoderl\_wm to pull in webmachine dependency

### oc\_erchef 0.23.0
* [CHEF-4086] Add configurable host for S3 pre-signed URLs
* Refactor chef\_objects, chef\_db, and chef\_wm to support non-open-source features
* Add support for SQL/Erlang /containers endpoint (not migrated)
* Add support for SQL/Erlang /groups endpoint (not migrated)
* Convert all configuration fetching code to use envy library
* Remove REST API for darklaunch
* Add containers API docs to oc\_erchef code base
* Remove caching of search-related database responses
* Remove fast\_log and replace with lager
* Add IPV6 support
* Differentiate between 404s for missing principal vs. missing org

### opscode-account rel-1.43.0
* Remove SQL switching code for migrated objects
* Support container objects in SQL
* Support group objects in SQL
* Remove obsolete clients controller
* Encrypt user passwords with bcrypt
* BUGFIX: allow non-admin users to leave organizations
* Remove UPDATE from containers API
* Add IPV6 support
* BUGFIX: fix Ace.new method in #update\_user\_ace
* BUGFIX: don't log password changes in plain text
* BUGFIX: /organizations API can't show billing admins group

### sqitch
* Ensure sqitch uses an Omnibus-specific CPAN config

### private-chef-cookbooks
* [keepalived] Adjust command syntax for 1.2.9
* [erchef / bookshelf] Add s3\_external\_url configuration
* [all] Add IPV6 address support
* [nginx] Add ipv6only option to listen directive
* [sysctl] Force net.ipv6.bindonly to 0
* [opscode-certificate] Run certificate service on front-ends
* [redis] Add redis back into EC build (name redis-lb)
* [enterprise-chef-server-schema] Add schema upgrade for bcrypt user password support
* [openresty] Add lua-based upstream routing
* [oc\_bifrost] Use opscoderl\_wm logging
* [oc\_erchef] Replace fast\_log with lager
* [oc\_erchef] Remove deprecated use of db\_type for sqerl config
* [configuration] Increment api\_version for release 11.0.0 -> 11.1.0
* [opscode-certificate] Make sure :restart action occurs on all nodes
* [keepalived] Fixes for keepalived.conf to work with 1.2.9 unicast
* [bookshelf] Turn off request logging