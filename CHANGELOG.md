# Chef Server Changelog

## (unknown)

### private-chef-cookbooks/private-chef
- Fix logrotate configuration to make it work with SELinux enabled.

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
