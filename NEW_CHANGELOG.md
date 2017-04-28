# Change Log

## [12.15.0](https://github.com/chef/chef-server/tree/12.15.0) (2017-04-27)
[Full Changelog](https://github.com/chef/chef-server/compare/12.14.0...12.15.0)

**Implemented enhancements:**

- LDAP: Synthesize a displayname from surname, givenname [\#151](https://github.com/chef/chef-server/issues/151)
- Mapping Capability Between LDAP and Chef Server 12 Attributes [\#104](https://github.com/chef/chef-server/issues/104)
- Enhance API to handle global groups in local contexts [\#1159](https://github.com/chef/chef-server/pull/1159) ([markan](https://github.com/markan))

**Closed issues:**

- t [\#1234](https://github.com/chef/chef-server/issues/1234)
- org-create not executed successfully [\#1231](https://github.com/chef/chef-server/issues/1231)
- Failure upgrading RabbitMQ from Migration 1.30 to 1.31 [\#1220](https://github.com/chef/chef-server/issues/1220)
- chef-sever 12.14 HA erchef keeps looking for rabbitmq process... [\#1212](https://github.com/chef/chef-server/issues/1212)
- Installing opscode-reporting on chef-server v12.17.44 breaks chef irreparably [\#1208](https://github.com/chef/chef-server/issues/1208)
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

## [12.14.0](https://github.com/chef/chef-server/tree/12.14.0) (2017-03-30)
[Full Changelog](https://github.com/chef/chef-server/compare/12.13.0...12.14.0)

**Fixed bugs:**

- Unset/reset SVDIR inside chef-server-ctl [\#1075](https://github.com/chef/chef-server/issues/1075)
- chef-server-ctl password \<username\> : Don't spit password to screen [\#748](https://github.com/chef/chef-server/issues/748)

**Closed issues:**

- Create encrypted data bag items from API REST [\#1164](https://github.com/chef/chef-server/issues/1164)
-  POST action on ACL endpoint doesn't work [\#1127](https://github.com/chef/chef-server/issues/1127)
- chef-backend-ctl create-cluster creates broken cluster if ipv6 is turned on [\#1111](https://github.com/chef/chef-server/issues/1111)
- Minor issue tracking \(chef\_secrets conversion\) [\#1108](https://github.com/chef/chef-server/issues/1108)
- 12.7+ breaks nginx url [\#922](https://github.com/chef/chef-server/issues/922)
- Organizations starting with "bookshelf" are unusable [\#694](https://github.com/chef/chef-server/issues/694)
- chef-server-ctl tail: cannot follow ‘-’ by name [\#672](https://github.com/chef/chef-server/issues/672)
- gmake omnibus failed during build chef-server [\#636](https://github.com/chef/chef-server/issues/636)
- Sporatic build failures in chef\_index\_batch tests [\#631](https://github.com/chef/chef-server/issues/631)
-  chef-server-ctl org-create is broken in 12.4.0.rc.2 [\#356](https://github.com/chef/chef-server/issues/356)
- chef-server-ctl test failure: Search API endpoint using POST  [\#321](https://github.com/chef/chef-server/issues/321)
- Support upgrades to 12 from older Chef 11 versions [\#182](https://github.com/chef/chef-server/issues/182)

**Merged pull requests:**

- Fix chef-server-ctl test for opsworks [\#1233](https://github.com/chef/chef-server/pull/1233) ([markan](https://github.com/markan))
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
- Release Chef Server 12.14.0 [\#1181](https://github.com/chef/chef-server/pull/1181) ([stevendanna](https://github.com/stevendanna))
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

**Fixed bugs:**

- Backups created by chef-server-ctl backup command cannot be restored after version 12.9.1-1 [\#1020](https://github.com/chef/chef-server/issues/1020)
- chef-server-ctl install fails on Amazon Linux [\#741](https://github.com/chef/chef-server/issues/741)

**Closed issues:**

- Chef rotate all keys -- rotates a deleted key [\#1079](https://github.com/chef/chef-server/issues/1079)
- Partial Search Invalid JSON [\#1071](https://github.com/chef/chef-server/issues/1071)
- Chef-Server error creating user [\#879](https://github.com/chef/chef-server/issues/879)
- Omnibus builds requires nodejs from host [\#813](https://github.com/chef/chef-server/issues/813)
- Failed to upload cookbook onto S3 [\#801](https://github.com/chef/chef-server/issues/801)

**Merged pull requests:**

- Release 12.13.0 [\#1093](https://github.com/chef/chef-server/pull/1093) ([srenatus](https://github.com/srenatus))
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

## [12.12.0](https://github.com/chef/chef-server/tree/12.12.0) (2017-01-27)
[Full Changelog](https://github.com/chef/chef-server/compare/12.11.1...12.12.0)

**Implemented enhancements:**

- Add knife-ec-backup to core [\#1036](https://github.com/chef/chef-server/issues/1036)
- Confusing error message with duplicate email address [\#59](https://github.com/chef/chef-server/issues/59)

**Fixed bugs:**

- chef-server-ctl password command does not accept special characters  [\#366](https://github.com/chef/chef-server/issues/366)
- `chef-server-ctl cleanse` removes plugin config, breaking reconfigure [\#115](https://github.com/chef/chef-server/issues/115)
- Chef Server API should not allow usernames with spaces [\#90](https://github.com/chef/chef-server/issues/90)

**Closed issues:**

- Chef LDAP config breaks permissions [\#1058](https://github.com/chef/chef-server/issues/1058)
- enable fips mode with  chef-server-fips-core package [\#1024](https://github.com/chef/chef-server/issues/1024)
- error in chef-server-ctl reconfigure in docker. [\#960](https://github.com/chef/chef-server/issues/960)
- Data bag search doesn't work correctly for encrypted arrays [\#876](https://github.com/chef/chef-server/issues/876)
- push jobs won't install on vanilla chef server [\#842](https://github.com/chef/chef-server/issues/842)
- Error while installing Chef server 12.3 on Red hat 6.7 [\#681](https://github.com/chef/chef-server/issues/681)
- rebar3 warning during build [\#630](https://github.com/chef/chef-server/issues/630)
- oc\_id: email configuration [\#547](https://github.com/chef/chef-server/issues/547)
- It is possible to gather sensitive debugging information from Chef server's application error [\#194](https://github.com/chef/chef-server/issues/194)
- Add settings to production.yml for sending mail [\#185](https://github.com/chef/chef-server/issues/185)

**Merged pull requests:**

- Fail if files include UTF-8 characters. [\#1067](https://github.com/chef/chef-server/pull/1067) ([marcparadise](https://github.com/marcparadise))
- Log service start exit code. Use status to verify started service [\#1066](https://github.com/chef/chef-server/pull/1066) ([marcparadise](https://github.com/marcparadise))
- Release version 12.12.0 [\#1064](https://github.com/chef/chef-server/pull/1064) ([stevendanna](https://github.com/stevendanna))
- Bump omnibus-software to roll back to runit 2.1.1 [\#1063](https://github.com/chef/chef-server/pull/1063) ([stevendanna](https://github.com/stevendanna))
- Minor README changes [\#1062](https://github.com/chef/chef-server/pull/1062) ([stevendanna](https://github.com/stevendanna))
- Update omnibus-software to get the latest server-jre [\#1059](https://github.com/chef/chef-server/pull/1059) ([stevendanna](https://github.com/stevendanna))
- Fix the fips build [\#1056](https://github.com/chef/chef-server/pull/1056) ([stevendanna](https://github.com/stevendanna))
- Remove coverdata files from oc\_erchef dir [\#1055](https://github.com/chef/chef-server/pull/1055) ([stevendanna](https://github.com/stevendanna))
- Fix Makefiles to not distclean by default [\#1054](https://github.com/chef/chef-server/pull/1054) ([stevendanna](https://github.com/stevendanna))
- Update depselector\_rb Gemfile to use HTTPS [\#1053](https://github.com/chef/chef-server/pull/1053) ([stevendanna](https://github.com/stevendanna))
- Revert ruby to 2.2.6 [\#1052](https://github.com/chef/chef-server/pull/1052) ([stevendanna](https://github.com/stevendanna))
- Make oc-chef-pedant wait for queues to be empty in reindex tests [\#1051](https://github.com/chef/chef-server/pull/1051) ([srenatus](https://github.com/srenatus))
- \[SPOOL-490\] Make java handle GC log rotation [\#1050](https://github.com/chef/chef-server/pull/1050) ([srenatus](https://github.com/srenatus))
- Lock chef-client and ohai to 12.17.44 and 8.22.1 [\#1049](https://github.com/chef/chef-server/pull/1049) ([marcparadise](https://github.com/marcparadise))
- Update maintainers with recent changes [\#1048](https://github.com/chef/chef-server/pull/1048) ([marcparadise](https://github.com/marcparadise))
- Move to Ruby 2.3.3 [\#1046](https://github.com/chef/chef-server/pull/1046) ([stevendanna](https://github.com/stevendanna))
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

## [12.11.1](https://github.com/chef/chef-server/tree/12.11.1) (2016-11-18)
[Full Changelog](https://github.com/chef/chef-server/compare/12.11.0...12.11.1)

**Closed issues:**

- execute\("initctl status private-chef-runsvdir"\) do [\#1015](https://github.com/chef/chef-server/issues/1015)

**Merged pull requests:**

- Release Chef Server 12.11.1 [\#1018](https://github.com/chef/chef-server/pull/1018) ([stevendanna](https://github.com/stevendanna))
- Update omnibus-software to latest for node s390x fix [\#1017](https://github.com/chef/chef-server/pull/1017) ([smith](https://github.com/smith))
- Loosen TLS config for pushy-server 1.x [\#1016](https://github.com/chef/chef-server/pull/1016) ([stevendanna](https://github.com/stevendanna))

## [12.11.0](https://github.com/chef/chef-server/tree/12.11.0) (2016-11-14)
[Full Changelog](https://github.com/chef/chef-server/compare/12.10.0...12.11.0)

**Implemented enhancements:**

- LDAP mail field is not configurable [\#1000](https://github.com/chef/chef-server/issues/1000)
- About to choose an attribute as displayname when authenticating with LDAP [\#971](https://github.com/chef/chef-server/issues/971)
- Hosted Chef Should Allow Log Export or Streaming Through an API [\#145](https://github.com/chef/chef-server/issues/145)
- Chef-sync should sync users [\#134](https://github.com/chef/chef-server/issues/134)
- data-collector rewrite based on the path of root\_url attribute [\#1012](https://github.com/chef/chef-server/pull/1012) ([alexpop](https://github.com/alexpop))

**Fixed bugs:**

- Failed to restore backup data using chef-backend-ctl 1.1.2 [\#926](https://github.com/chef/chef-server/issues/926)
- Embedded Logrotate Not Rotating Nginx Access Logfiles [\#843](https://github.com/chef/chef-server/issues/843)
- rabbitmq\['management\_enabled'\] setting doesn't actually do anything [\#688](https://github.com/chef/chef-server/issues/688)
- Installation fails in Ubuntu 15.04 and later \(/usr/lib/systemd/system does not exist\) [\#572](https://github.com/chef/chef-server/issues/572)
- ORG\_FULL\_NAME argument to org-create is parsed wrong [\#354](https://github.com/chef/chef-server/issues/354)
- Postgres fails to start due to too large a shared memory setting [\#211](https://github.com/chef/chef-server/issues/211)
- A Chef Server Backup System Should Be Productized and Bundled [\#107](https://github.com/chef/chef-server/issues/107)
- Chef Server Analytics Queue Depth Should Be Capped When No Consumer Present [\#103](https://github.com/chef/chef-server/issues/103)
- chef-server 12 on RHEL7: chef-server-ctl reconfigure hangs when configuring embedded rabbitmq [\#62](https://github.com/chef/chef-server/issues/62)
- Chef 12 Logging Passwords [\#42](https://github.com/chef/chef-server/issues/42)
- Rewrite the POST to the correct Automate endpoint [\#1010](https://github.com/chef/chef-server/pull/1010) ([alexpop](https://github.com/alexpop))

**Closed issues:**

- chef-server-ctl reconfigure fails when ldap\['bind\_password'\] is not set [\#947](https://github.com/chef/chef-server/issues/947)
- deb package of bad quality on install [\#934](https://github.com/chef/chef-server/issues/934)
- chef-manage-ctl hangs with chef server version 12  [\#933](https://github.com/chef/chef-server/issues/933)
- \[knife upload\] ERROR: Server returned error 500 for sandboxes [\#864](https://github.com/chef/chef-server/issues/864)
- chef repo should be configured as skippable [\#849](https://github.com/chef/chef-server/issues/849)
- Wrong error message `chef-server-ctl user-create` [\#845](https://github.com/chef/chef-server/issues/845)
- chef-sync 1.0-rc6 crashes [\#831](https://github.com/chef/chef-server/issues/831)
- Readline module missing in embedded Ruby in chef server 12.5.0 [\#810](https://github.com/chef/chef-server/issues/810)
- chef/bootstrapping [\#785](https://github.com/chef/chef-server/issues/785)
- chef-server-ctl commands hangs [\#773](https://github.com/chef/chef-server/issues/773)
- Update default TLS versions config [\#738](https://github.com/chef/chef-server/issues/738)
- Use of the word "role" as a key in a Chef attribute can cause nodes to be misidentified [\#718](https://github.com/chef/chef-server/issues/718)
- Bad lines sent when statsd stats are enabled [\#679](https://github.com/chef/chef-server/issues/679)
- OmnibusHelper.normalize\_host treats "host:port" as IPv6 address [\#665](https://github.com/chef/chef-server/issues/665)
- DELETE /policy\_groups/x/policies/y can leave an empty group [\#654](https://github.com/chef/chef-server/issues/654)
- Not able to add domain user to organization [\#650](https://github.com/chef/chef-server/issues/650)
- Unable to set topology from dna.json file [\#583](https://github.com/chef/chef-server/issues/583)
- Missing opscode-push-jobs-server build for RHEL 7 [\#546](https://github.com/chef/chef-server/issues/546)
- First 'chef-server-ctl install' makes yum repo files w/o proxy info and fails [\#525](https://github.com/chef/chef-server/issues/525)
- Disabling LDAP requires editing the node directly [\#510](https://github.com/chef/chef-server/issues/510)
- 403 Forbidden on new nodes  [\#499](https://github.com/chef/chef-server/issues/499)
- can't install add-ons in 12.1.1 over command-line [\#434](https://github.com/chef/chef-server/issues/434)
- ssl err while configuring chef server 12 [\#428](https://github.com/chef/chef-server/issues/428)
- network connections on chef-server [\#412](https://github.com/chef/chef-server/issues/412)
- chef-sync should have option to sync only cookbook, or something else [\#394](https://github.com/chef/chef-server/issues/394)
- cwe-200 information exposure due to ssl and multiple names/addresses to hit the server [\#288](https://github.com/chef/chef-server/issues/288)
- Provide rabbitmq management commands for debugging and repair [\#239](https://github.com/chef/chef-server/issues/239)
- Installing add-ons on Amazon Linux fail [\#189](https://github.com/chef/chef-server/issues/189)
- chef-sync crashes after 'yum update' on RHEL 6.6 [\#158](https://github.com/chef/chef-server/issues/158)
- Chef Manage install fails with use of apt.conf.d proxy [\#52](https://github.com/chef/chef-server/issues/52)
- Confusing error message if hostname cannot be found in configuration file [\#49](https://github.com/chef/chef-server/issues/49)
- LDAP system recovery password set up error [\#47](https://github.com/chef/chef-server/issues/47)
- Installing local packages is indeterminate if you have multiple packages of the same version in the install dir [\#23](https://github.com/chef/chef-server/issues/23)

**Merged pull requests:**

- Release version 12.11.0 [\#1009](https://github.com/chef/chef-server/pull/1009) ([stevendanna](https://github.com/stevendanna))
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
- Add validation endpoint to Chef Server. [\#982](https://github.com/chef/chef-server/pull/982) ([markan](https://github.com/markan))

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

- 12.10.0 version bump and release notes. [\#989](https://github.com/chef/chef-server/pull/989) ([marcparadise](https://github.com/marcparadise))
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

## [12.9.1](https://github.com/chef/chef-server/tree/12.9.1) (2016-09-26)
[Full Changelog](https://github.com/chef/chef-server/compare/12.9.0...12.9.1)

**Merged pull requests:**

- Prepare for 12.9.1 release [\#946](https://github.com/chef/chef-server/pull/946) ([jonsmorrow](https://github.com/jonsmorrow))
- Updates omnibus software to latest [\#945](https://github.com/chef/chef-server/pull/945) ([jonsmorrow](https://github.com/jonsmorrow))
- Fix typo [\#941](https://github.com/chef/chef-server/pull/941) ([martinmosegaard](https://github.com/martinmosegaard))
- Fix CHEFDK\_GECODE\_PATH for latest ChefDK [\#940](https://github.com/chef/chef-server/pull/940) ([stevendanna](https://github.com/stevendanna))

## [12.9.0](https://github.com/chef/chef-server/tree/12.9.0) (2016-09-22)
[Full Changelog](https://github.com/chef/chef-server/compare/12.8.0...12.9.0)

**Fixed bugs:**

- Make activesupport dependency less restrictive [\#930](https://github.com/chef/chef-server/pull/930) ([rhass](https://github.com/rhass))

**Closed issues:**

- chef-server-ctl fails on user-create command [\#923](https://github.com/chef/chef-server/issues/923)
- unbale to fix the rabbitmq issue while executing 'sudo chef-server-ctl reconfigure' [\#916](https://github.com/chef/chef-server/issues/916)
- Chef-server-ctrl fails on any command [\#915](https://github.com/chef/chef-server/issues/915)
- Searches don't respect ACL read permission [\#897](https://github.com/chef/chef-server/issues/897)
- trying to add reporting onto my chef HA servers [\#892](https://github.com/chef/chef-server/issues/892)
- chef-server-ctl reconfigure with HA back-end [\#890](https://github.com/chef/chef-server/issues/890)
- Ubuntu 14.04 chef-server 12.8.0 - Recipe Compile Error on chef-server-ctl reconfigure [\#877](https://github.com/chef/chef-server/issues/877)
- chef-server 12.6.0 does not include cleanup script for opscode-expander-reindexer [\#846](https://github.com/chef/chef-server/issues/846)
- mover\_server\_admins\_global\_group\_callback: duplicate key [\#822](https://github.com/chef/chef-server/issues/822)
- Expose policy name and group on node object and search [\#436](https://github.com/chef/chef-server/issues/436)
- Erchef dumps LDAP password [\#156](https://github.com/chef/chef-server/issues/156)
- 400 Bad Request trying to grant permissions to client when user has same name [\#111](https://github.com/chef/chef-server/issues/111)

**Merged pull requests:**

- Prepare for 12.9.0 release [\#939](https://github.com/chef/chef-server/pull/939) ([marcparadise](https://github.com/marcparadise))
- Update README so changes to oc\_reporting get synced correctly. [\#937](https://github.com/chef/chef-server/pull/937) ([rmoshier](https://github.com/rmoshier))
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

- Release notes and version bump for 12.8.0 [\#875](https://github.com/chef/chef-server/pull/875) ([stevendanna](https://github.com/stevendanna))
- \[omnibus\] Update omnibus-software for libarchive config\_guess fix [\#873](https://github.com/chef/chef-server/pull/873) ([stevendanna](https://github.com/stevendanna))
- \[omnibus\] Move from berkshelf2 to latest bookshelf [\#872](https://github.com/chef/chef-server/pull/872) ([stevendanna](https://github.com/stevendanna))
- New rack requires updating chef-zero to 4.7 [\#871](https://github.com/chef/chef-server/pull/871) ([markan](https://github.com/markan))
- Fix repo for manderson26-\>markan git change [\#870](https://github.com/chef/chef-server/pull/870) ([markan](https://github.com/markan))
- \[ET-221\] Move SAML/LDAP check into pre-flight [\#868](https://github.com/chef/chef-server/pull/868) ([chefsalim](https://github.com/chefsalim))
- \[IPO-204\] Send actions to the Data Collector before sending stats\_her… [\#867](https://github.com/chef/chef-server/pull/867) ([ryancragun](https://github.com/ryancragun))
- Fix logging in server\_admins\_existing\_users\_read\_permissions [\#866](https://github.com/chef/chef-server/pull/866) ([stevendanna](https://github.com/stevendanna))
- \[IPO-203\] Update oc\_chef\_wm to send actions to the Data Collector [\#865](https://github.com/chef/chef-server/pull/865) ([ryancragun](https://github.com/ryancragun))
- \[IPO-202\] Add initial Data Collector application and /\_status check [\#858](https://github.com/chef/chef-server/pull/858) ([ryancragun](https://github.com/ryancragun))

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

- add common\_name, cn ldap binding, to auth lookup fields [\#863](https://github.com/chef/chef-server/pull/863) ([wrightp](https://github.com/wrightp))
- Update misleading filename error message [\#862](https://github.com/chef/chef-server/pull/862) ([MichaelPereira](https://github.com/MichaelPereira))
- release notes for chef-server 12.7.0 [\#861](https://github.com/chef/chef-server/pull/861) ([srenatus](https://github.com/srenatus))
- Add ci/run\_tests.sh to drive the CI process [\#859](https://github.com/chef/chef-server/pull/859) ([jkeiser](https://github.com/jkeiser))
- \[ET-202\] Fix chef\_manage node attribute access [\#856](https://github.com/chef/chef-server/pull/856) ([srenatus](https://github.com/srenatus))
- Update openresty to point to ppc64 lua location [\#855](https://github.com/chef/chef-server/pull/855) ([scotthain](https://github.com/scotthain))
- \[ET-202\] Check for SAML enablement during reconfigure [\#854](https://github.com/chef/chef-server/pull/854) ([chefsalim](https://github.com/chefsalim))
- Updated omnibus software pinning to pick up ppc64 friendly defs [\#853](https://github.com/chef/chef-server/pull/853) ([scotthain](https://github.com/scotthain))
- oc\_erchef users list: allow filtering by external\_authentication\_id [\#852](https://github.com/chef/chef-server/pull/852) ([sdelano](https://github.com/sdelano))
- Fix whitespace in config [\#851](https://github.com/chef/chef-server/pull/851) ([jkeiser](https://github.com/jkeiser))
- use chef\_zero mode in vagrant for dvm [\#850](https://github.com/chef/chef-server/pull/850) ([sdelano](https://github.com/sdelano))
- Use enterprise cookbook version that supports systemd on ubuntu 16.04 [\#848](https://github.com/chef/chef-server/pull/848) ([yzl](https://github.com/yzl))
- Reset initialization\_options and vendor\_class after a chef\_run [\#841](https://github.com/chef/chef-server/pull/841) ([ryancragun](https://github.com/ryancragun))
- Add chef-server-ctl require-credential-rotation command [\#840](https://github.com/chef/chef-server/pull/840) ([ryancragun](https://github.com/ryancragun))
- Update to pick up latest omnibus and omnibus software [\#839](https://github.com/chef/chef-server/pull/839) ([mmzyk](https://github.com/mmzyk))
- Remove chef-sync from the known add on packages for the install command [\#838](https://github.com/chef/chef-server/pull/838) ([mmzyk](https://github.com/mmzyk))
- release process updates [\#836](https://github.com/chef/chef-server/pull/836) ([wrightp](https://github.com/wrightp))
- \[omnibus\] bypass\_bootstrap? should ensure both creds exist [\#835](https://github.com/chef/chef-server/pull/835) ([stevendanna](https://github.com/stevendanna))
- Add Ryan Cragun as a Chef Server maintainer [\#834](https://github.com/chef/chef-server/pull/834) ([ryancragun](https://github.com/ryancragun))
- Fixing pedant/bookshelf when nginx on non-standard port [\#833](https://github.com/chef/chef-server/pull/833) ([adamleff](https://github.com/adamleff))
- Update opscode-solr4 JAVA\_OPTS to include whitespace [\#830](https://github.com/chef/chef-server/pull/830) ([brentm5](https://github.com/brentm5))
- Update chef-server release process documentation. [\#829](https://github.com/chef/chef-server/pull/829) ([rmoshier](https://github.com/rmoshier))
- Release Process Updates [\#828](https://github.com/chef/chef-server/pull/828) ([schisamo](https://github.com/schisamo))
- Add support for service credentials rotation [\#798](https://github.com/chef/chef-server/pull/798) ([ryancragun](https://github.com/ryancragun))
- Updated Copyright and URL [\#771](https://github.com/chef/chef-server/pull/771) ([jjasghar](https://github.com/jjasghar))

## [12.6.0](https://github.com/chef/chef-server/tree/12.6.0) (2016-04-30)
[Full Changelog](https://github.com/chef/chef-server/compare/12.5.0...12.6.0)

**Closed issues:**

- chef-server-ctl grant-server-admin-permissions needs cli help [\#806](https://github.com/chef/chef-server/issues/806)
- chef-server-ctl cannot load such file -- chef/key \(LoadError\) [\#632](https://github.com/chef/chef-server/issues/632)

**Merged pull requests:**

- Finalize 12.6.0 release. [\#827](https://github.com/chef/chef-server/pull/827) ([tylercloke](https://github.com/tylercloke))
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

- Release chef-server 12.5.0 [\#791](https://github.com/chef/chef-server/pull/791) ([ksubrama](https://github.com/ksubrama))
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
- Added /orgs/org/users/user/keys\(/key\) endpoints and changed default perms on org scoped key GETs. [\#769](https://github.com/chef/chef-server/pull/769) ([tylercloke](https://github.com/tylercloke))
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

## [12.4.1](https://github.com/chef/chef-server/tree/12.4.1) (2016-02-03)
[Full Changelog](https://github.com/chef/chef-server/compare/12.4.0...12.4.1)

**Fixed bugs:**

- chef-server-ctl upgrade broken in 12.4.0 [\#724](https://github.com/chef/chef-server/issues/724)
- Create cookbook artifacts with all fields filled in [\#714](https://github.com/chef/chef-server/pull/714) ([danielsdeleo](https://github.com/danielsdeleo))

## [12.4.0](https://github.com/chef/chef-server/tree/12.4.0) (2016-01-27)
[Full Changelog](https://github.com/chef/chef-server/compare/12.3.1...12.4.0)

**Fixed bugs:**

- Set rabbitmq\_management listener IP to `node\['private\_chef'\]\['rabbitmq'\]\['node\_ip\_address'\]` [\#686](https://github.com/chef/chef-server/issues/686)

**Closed issues:**

- chef-server-core on Ubuntu 15.10 [\#682](https://github.com/chef/chef-server/issues/682)
- Issue with Postgres DB? [\#678](https://github.com/chef/chef-server/issues/678)
- Internal Server Error during knife upload [\#674](https://github.com/chef/chef-server/issues/674)
- Can't install opscode-reporting [\#661](https://github.com/chef/chef-server/issues/661)
- 500 errors in resolving cookbook for runlist - bad argument in call to erlang in chef\_depsolver\_worker [\#648](https://github.com/chef/chef-server/issues/648)
- Can't upload cookbook when server uses a non-standard port [\#634](https://github.com/chef/chef-server/issues/634)
- backend postgresql preflight validation runs on front-end nodes.  [\#599](https://github.com/chef/chef-server/issues/599)
- Cannot upload cookbooks in enable\_non\_ssl\_port true - error 500 internal server error [\#473](https://github.com/chef/chef-server/issues/473)
- Can't delete a user, no chef-server-ctl or knife commands for group operations? [\#157](https://github.com/chef/chef-server/issues/157)
- opscode-expander-reindexer can likely be removed [\#30](https://github.com/chef/chef-server/issues/30)

**Merged pull requests:**

- Updating changelog for 12.4.0 release [\#712](https://github.com/chef/chef-server/pull/712) ([dmccown](https://github.com/dmccown))
- \[bookshelf\] Return 204 on cookbook upload [\#711](https://github.com/chef/chef-server/pull/711) ([stevendanna](https://github.com/stevendanna))
- Update gemfile.lock to pull in newer rails due to security incident [\#708](https://github.com/chef/chef-server/pull/708) ([kmacgugan](https://github.com/kmacgugan))
- Add user\_keys, client\_keys tags in oc-chef-pedant keys\_spec.rb [\#707](https://github.com/chef/chef-server/pull/707) ([jrunning](https://github.com/jrunning))
- Fix rspec failures for BookshelfPreflightValidator [\#706](https://github.com/chef/chef-server/pull/706) ([stevendanna](https://github.com/stevendanna))
- \[bookshelf\] Ensure bookshelf db is properly validated and cleaned up [\#705](https://github.com/chef/chef-server/pull/705) ([stevendanna](https://github.com/stevendanna))
- HA-5 - standalone front-end bootstrapping  [\#704](https://github.com/chef/chef-server/pull/704) ([marcparadise](https://github.com/marcparadise))
- Add validation tag to header validation test [\#703](https://github.com/chef/chef-server/pull/703) ([danielsdeleo](https://github.com/danielsdeleo))
- Allow opt-out of RVM/bundler busting in knife pedant tests [\#702](https://github.com/chef/chef-server/pull/702) ([danielsdeleo](https://github.com/danielsdeleo))
- Set rabbitmq\_management listener IP to rabbitmq node\_ip\_address [\#701](https://github.com/chef/chef-server/pull/701) ([jeremiahsnapp](https://github.com/jeremiahsnapp))
- Add test for knife-opc org creation [\#700](https://github.com/chef/chef-server/pull/700) ([jaym](https://github.com/jaym))
- Add ACLs to policy groups [\#699](https://github.com/chef/chef-server/pull/699) ([jkeiser](https://github.com/jkeiser))
- Add control groups schemas to API docs [\#698](https://github.com/chef/chef-server/pull/698) ([jkeiser](https://github.com/jkeiser))
- WIP - Correct handling of no rabbitmq in controls endpoint [\#697](https://github.com/chef/chef-server/pull/697) ([rmoshier](https://github.com/rmoshier))
- Populate fields that a normal cookbook upload would populate [\#696](https://github.com/chef/chef-server/pull/696) ([jkeiser](https://github.com/jkeiser))
- Add SQL storage support to bookshelf [\#693](https://github.com/chef/chef-server/pull/693) ([stevendanna](https://github.com/stevendanna))
- Add a flag to ignore quirks specific to chef-zero \(like name size = 255\) [\#692](https://github.com/chef/chef-server/pull/692) ([jkeiser](https://github.com/jkeiser))
- Modify postgres preflight checks to have correct assumptions [\#691](https://github.com/chef/chef-server/pull/691) ([rmoshier](https://github.com/rmoshier))
- Ssd/bookshelf on pgsql4 [\#685](https://github.com/chef/chef-server/pull/685) ([stevendanna](https://github.com/stevendanna))
- Fix statem test output formatting [\#683](https://github.com/chef/chef-server/pull/683) ([metadave](https://github.com/metadave))
- Use single branch for regular chef server and the one with openssl in fips mode [\#680](https://github.com/chef/chef-server/pull/680) ([jaym](https://github.com/jaym))
- Simplify bootstrap, move pivotal key gen,  pg retries [\#677](https://github.com/chef/chef-server/pull/677) ([marcparadise](https://github.com/marcparadise))
- Fix up some pedant spec descriptions that lagged behind API developments [\#673](https://github.com/chef/chef-server/pull/673) ([randomcamel](https://github.com/randomcamel))
- Automated reporting matrix testing [\#671](https://github.com/chef/chef-server/pull/671) ([tylercloke](https://github.com/tylercloke))
- FLOW-67: ACL endpoints for policies and policy groups [\#667](https://github.com/chef/chef-server/pull/667) ([doubt72](https://github.com/doubt72))
- groups, principals, search and org members endpoints [\#660](https://github.com/chef/chef-server/pull/660) ([jkeiser](https://github.com/jkeiser))
- Add nodes endpoint methods [\#657](https://github.com/chef/chef-server/pull/657) ([jkeiser](https://github.com/jkeiser))
- Compare parsed json, not exact body-with-whitespace [\#655](https://github.com/chef/chef-server/pull/655) ([jkeiser](https://github.com/jkeiser))
- Implement RFC 14 - Add universe endpoint [\#645](https://github.com/chef/chef-server/pull/645) ([thommay](https://github.com/thommay))
- Organize RAML and add all endpoints [\#644](https://github.com/chef/chef-server/pull/644) ([jkeiser](https://github.com/jkeiser))
- Remove bksw\_sync module [\#642](https://github.com/chef/chef-server/pull/642) ([stevendanna](https://github.com/stevendanna))
- don't monitor rabbit queue length w/ actions disabled [\#641](https://github.com/chef/chef-server/pull/641) ([sdelano](https://github.com/sdelano))
- 12.3.1 - set the package version number [\#640](https://github.com/chef/chef-server/pull/640) ([sdelano](https://github.com/sdelano))
- Restrict 'other' permissions for chef-server.rb. [\#639](https://github.com/chef/chef-server/pull/639) ([itmustbejj](https://github.com/itmustbejj))
- Remove unused components/config [\#635](https://github.com/chef/chef-server/pull/635) ([stevendanna](https://github.com/stevendanna))
- Typo fixes [\#625](https://github.com/chef/chef-server/pull/625) ([smith](https://github.com/smith))
- Make sure chef-server-ctl install can do chef-manage [\#558](https://github.com/chef/chef-server/pull/558) ([jcreedcmu](https://github.com/jcreedcmu))
- Implemented Version 1 of Server Admins [\#475](https://github.com/chef/chef-server/pull/475) ([tylercloke](https://github.com/tylercloke))

## [12.3.1](https://github.com/chef/chef-server/tree/12.3.1) (2015-11-19)
[Full Changelog](https://github.com/chef/chef-server/compare/12.3.0...12.3.1)

**Merged pull requests:**

- Pin rebar3\_neotoma\_plugin until we can upgrade to rebar3-beta4 [\#629](https://github.com/chef/chef-server/pull/629) ([stevendanna](https://github.com/stevendanna))
- update minimal cookbook versions query to force query plan [\#627](https://github.com/chef/chef-server/pull/627) ([sdelano](https://github.com/sdelano))
- Raml docs for cookbook artifacts [\#626](https://github.com/chef/chef-server/pull/626) ([danielsdeleo](https://github.com/danielsdeleo))
- Pedant changes to support `chef-zero` [\#622](https://github.com/chef/chef-server/pull/622) ([randomcamel](https://github.com/randomcamel))

## [12.3.0](https://github.com/chef/chef-server/tree/12.3.0) (2015-11-12)
[Full Changelog](https://github.com/chef/chef-server/compare/12.2.0...12.3.0)

**Implemented enhancements:**

- Enterprise Chef / Chef Server 12: View Public Keys of all Users [\#20](https://github.com/chef/chef-server/issues/20)
- Gather Logs Updates [\#555](https://github.com/chef/chef-server/pull/555) ([sean-horn](https://github.com/sean-horn))

**Closed issues:**

- Error executing action `run` on resource 'execute\[bootstrap-platform\]' [\#598](https://github.com/chef/chef-server/issues/598)
- nginx won't start post installation [\#595](https://github.com/chef/chef-server/issues/595)
- Chef 12 opscode-reporting bootstrap is failing behind a proxy [\#590](https://github.com/chef/chef-server/issues/590)
- On 1st chef-server-ctl reconfigure execute\[bootstrap-platform\] fails [\#584](https://github.com/chef/chef-server/issues/584)
- chef-server-ctl reconfigure fails with custom nginx cert  [\#567](https://github.com/chef/chef-server/issues/567)
- chef-server 12.1.2 -\> 12.2.0 upgrade failed [\#549](https://github.com/chef/chef-server/issues/549)
- Unable to install chef-server on docker container [\#548](https://github.com/chef/chef-server/issues/548)
- Chef Server serves old cookbook versions [\#535](https://github.com/chef/chef-server/issues/535)
- chef-server-ctl backup and return code [\#534](https://github.com/chef/chef-server/issues/534)
- erchef / chef manage - status=412; "\>\>,"Precondition Failed" causes chef server to become unresponsive [\#526](https://github.com/chef/chef-server/issues/526)
- chef-sync crashes when installed from scratch [\#517](https://github.com/chef/chef-server/issues/517)
- Pivotal user used to update user name in chef server using PUT\_REST. [\#515](https://github.com/chef/chef-server/issues/515)
- Opscode Reporting "\[error\] Received invalid principal" [\#482](https://github.com/chef/chef-server/issues/482)
- I can't change postgresql port [\#459](https://github.com/chef/chef-server/issues/459)
- chef-server-ctl ha-status incorretly reports opscode-chef-mover as down in HA setup on primary [\#361](https://github.com/chef/chef-server/issues/361)
- Support SSO login [\#200](https://github.com/chef/chef-server/issues/200)

**Merged pull requests:**

- 12.3.0 release [\#618](https://github.com/chef/chef-server/pull/618) ([marcparadise](https://github.com/marcparadise))
- Bump omnibus-software to latest [\#616](https://github.com/chef/chef-server/pull/616) ([stevendanna](https://github.com/stevendanna))
- Mark policy/policy group validation specs with :validation. [\#615](https://github.com/chef/chef-server/pull/615) ([randomcamel](https://github.com/randomcamel))
- Add option to set rspec seed value [\#614](https://github.com/chef/chef-server/pull/614) ([markan](https://github.com/markan))
- Don't run eunit on mover's vendored code. [\#613](https://github.com/chef/chef-server/pull/613) ([marcparadise](https://github.com/marcparadise))
- Follow filename convention for migration 26 [\#612](https://github.com/chef/chef-server/pull/612) ([stevendanna](https://github.com/stevendanna))
- Don't create unused SERVICE/etc dirs for erlang services [\#611](https://github.com/chef/chef-server/pull/611) ([stevendanna](https://github.com/stevendanna))
- ChangeLog-Entry: \[dvm\] add support for dotfiles external to the repository [\#610](https://github.com/chef/chef-server/pull/610) ([marcparadise](https://github.com/marcparadise))
- missing dev template from PR 608 [\#609](https://github.com/chef/chef-server/pull/609) ([metadave](https://github.com/metadave))
- allow dvm to create an external reporting db vm [\#608](https://github.com/chef/chef-server/pull/608) ([metadave](https://github.com/metadave))
- nil username breaks Analytics login [\#606](https://github.com/chef/chef-server/pull/606) ([metadave](https://github.com/metadave))
- Mark every spec expecting a 400 as :validation. [\#600](https://github.com/chef/chef-server/pull/600) ([randomcamel](https://github.com/randomcamel))
- Add retries to execute\["bootstrap-platform"\] [\#597](https://github.com/chef/chef-server/pull/597) ([poliva83](https://github.com/poliva83))
- update rebar locks for bookshelf, bifrost, and erchef [\#592](https://github.com/chef/chef-server/pull/592) ([marcparadise](https://github.com/marcparadise))
- ChangeLog-Entry: \[oc\_erchef\] queue monitor doesn't affect overall\_sta… [\#591](https://github.com/chef/chef-server/pull/591) ([metadave](https://github.com/metadave))
- ElasticSearch Support [\#589](https://github.com/chef/chef-server/pull/589) ([stevendanna](https://github.com/stevendanna))
- dvm fixes to fix unhelpful error messages and enable successful loading of bifrost [\#588](https://github.com/chef/chef-server/pull/588) ([irvingpop](https://github.com/irvingpop))
- Avoid proxies when talking to local chef-server [\#579](https://github.com/chef/chef-server/pull/579) ([stevendanna](https://github.com/stevendanna))
- Mp/parameterize search provider [\#576](https://github.com/chef/chef-server/pull/576) ([marcparadise](https://github.com/marcparadise))
- \[dvm\] simplify sync [\#574](https://github.com/chef/chef-server/pull/574) ([marcparadise](https://github.com/marcparadise))
- \[dvm\] List installer packages starting at 1 not 0 [\#573](https://github.com/chef/chef-server/pull/573) ([stevendanna](https://github.com/stevendanna))
- Address CVE-2014-3628 [\#571](https://github.com/chef/chef-server/pull/571) ([pburkholder](https://github.com/pburkholder))
- rabbitmq queue monitor + queue stability enhancements [\#570](https://github.com/chef/chef-server/pull/570) ([metadave](https://github.com/metadave))
- Comment vendored depsolver test [\#569](https://github.com/chef/chef-server/pull/569) ([stevendanna](https://github.com/stevendanna))
- dvm powerdown ssh fail and more [\#566](https://github.com/chef/chef-server/pull/566) ([marcparadise](https://github.com/marcparadise))
- Adding configurability for erchef and bifrost logging messages per second [\#565](https://github.com/chef/chef-server/pull/565) ([shruthi-venkateswaran](https://github.com/shruthi-venkateswaran))
- Upgrade PostgreSQL to 9.2.14. [\#564](https://github.com/chef/chef-server/pull/564) ([rhass](https://github.com/rhass))
- Remove "hosted" from page copy [\#563](https://github.com/chef/chef-server/pull/563) ([smith](https://github.com/smith))
- Changes to allow login/password resets with email [\#560](https://github.com/chef/chef-server/pull/560) ([doubt72](https://github.com/doubt72))
- Warns rather than fail if external project isn't linked [\#556](https://github.com/chef/chef-server/pull/556) ([stevendanna](https://github.com/stevendanna))
- \[oc-chef-pedant\] Fix search tests incorrectly using with\_search\_polling [\#552](https://github.com/chef/chef-server/pull/552) ([stevendanna](https://github.com/stevendanna))
- Override bundler from omnibus-software default of 1.5.3 to 1.10.6. [\#550](https://github.com/chef/chef-server/pull/550) ([tylercloke](https://github.com/tylercloke))
- Use configured port everywhere we talk to postgres [\#545](https://github.com/chef/chef-server/pull/545) ([marcparadise](https://github.com/marcparadise))
- Add favicons for oc-id and static files. [\#543](https://github.com/chef/chef-server/pull/543) ([smith](https://github.com/smith))
- reporting dvm support and more  [\#542](https://github.com/chef/chef-server/pull/542) ([marcparadise](https://github.com/marcparadise))
- Upgrade opscoderl\_http to fix connection leak [\#541](https://github.com/chef/chef-server/pull/541) ([stevendanna](https://github.com/stevendanna))
- Use case-insensitive search for user lookup by external\_auth\_id [\#540](https://github.com/chef/chef-server/pull/540) ([stevendanna](https://github.com/stevendanna))
- Fix chef-server-ctl backup always returning 1 [\#537](https://github.com/chef/chef-server/pull/537) ([ryancragun](https://github.com/ryancragun))
- Don't consider opscode-chef-mover service status when checking ha-status [\#536](https://github.com/chef/chef-server/pull/536) ([poliva83](https://github.com/poliva83))
- stabilize chef\_index\_batch tests [\#533](https://github.com/chef/chef-server/pull/533) ([stevendanna](https://github.com/stevendanna))
- Fix the build [\#532](https://github.com/chef/chef-server/pull/532) ([tylercloke](https://github.com/tylercloke))
- Remove unused chef\_otto.hrl [\#529](https://github.com/chef/chef-server/pull/529) ([stevendanna](https://github.com/stevendanna))
- Avoid spurious HTTP 400 after GET to /\_status [\#528](https://github.com/chef/chef-server/pull/528) ([stevendanna](https://github.com/stevendanna))
- cleanup static nginx files [\#524](https://github.com/chef/chef-server/pull/524) ([marcparadise](https://github.com/marcparadise))
- upgrade jquery-rails to patch CVE-2015-1840 [\#522](https://github.com/chef/chef-server/pull/522) ([raskchanky](https://github.com/raskchanky))
- Support directly posting index updates to Solr [\#520](https://github.com/chef/chef-server/pull/520) ([stevendanna](https://github.com/stevendanna))
- Fix build of `chef\_backup` [\#519](https://github.com/chef/chef-server/pull/519) ([danielsdeleo](https://github.com/danielsdeleo))
- Lock port compiler to 0.3.0  [\#518](https://github.com/chef/chef-server/pull/518) ([stevendanna](https://github.com/stevendanna))
- Set forward\_agent = true in Vagrant ssh config [\#516](https://github.com/chef/chef-server/pull/516) ([stevendanna](https://github.com/stevendanna))
- Node policy integration [\#514](https://github.com/chef/chef-server/pull/514) ([danielsdeleo](https://github.com/danielsdeleo))
- Adding suse to package support for local addon installs. [\#509](https://github.com/chef/chef-server/pull/509) ([andy-dufour](https://github.com/andy-dufour))
- Remove oc\_erchef build artificats from git [\#508](https://github.com/chef/chef-server/pull/508) ([stevendanna](https://github.com/stevendanna))
- Alphabetize rebar.lock file [\#507](https://github.com/chef/chef-server/pull/507) ([stevendanna](https://github.com/stevendanna))

## [12.2.0](https://github.com/chef/chef-server/tree/12.2.0) (2015-09-02)
[Full Changelog](https://github.com/chef/chef-server/compare/12.1.2...12.2.0)

**Implemented enhancements:**

- Ability to Enable/Disable Packagecloud.io OS Repo [\#141](https://github.com/chef/chef-server/issues/141)

**Closed issues:**

- \* [\#503](https://github.com/chef/chef-server/issues/503)
- Prevent Unintended Updates for RHEL on existing systems [\#491](https://github.com/chef/chef-server/issues/491)
- opscode-reporting error 500 [\#480](https://github.com/chef/chef-server/issues/480)
- license endpoint should return number of nodes in existing orgs [\#437](https://github.com/chef/chef-server/issues/437)
- CS 12.1.0 nginx can't reach OC-ID if localhost resolves to IPv6 address [\#375](https://github.com/chef/chef-server/issues/375)
- opscode-manage fails to install on RHEL6 [\#332](https://github.com/chef/chef-server/issues/332)
- packagecloud yum repo metadata can break Chef Server if auto-update is enabled [\#154](https://github.com/chef/chef-server/issues/154)

**Merged pull requests:**

- update erlang project jenkins builds with omnibus-toolchain in path [\#513](https://github.com/chef/chef-server/pull/513) ([marcparadise](https://github.com/marcparadise))
- Mp/omnibus ctl 0.4.2 [\#512](https://github.com/chef/chef-server/pull/512) ([marcparadise](https://github.com/marcparadise))
- 12.2.0 release notes and changelog.  [\#511](https://github.com/chef/chef-server/pull/511) ([marcparadise](https://github.com/marcparadise))
- Update openssl cookbook dependency [\#505](https://github.com/chef/chef-server/pull/505) ([charlesjohnson](https://github.com/charlesjohnson))
- Updated rebar3 [\#504](https://github.com/chef/chef-server/pull/504) ([joedevivo](https://github.com/joedevivo))
- ensure gather-logs uses admin login, and add admin login psql option  [\#502](https://github.com/chef/chef-server/pull/502) ([marcparadise](https://github.com/marcparadise))
- Update jiffy from 0.6.1 to 0.14.1 [\#501](https://github.com/chef/chef-server/pull/501) ([stevendanna](https://github.com/stevendanna))
- ensure that chef-stable repository is disabled if present to prevent unintended updates [\#500](https://github.com/chef/chef-server/pull/500) ([marcparadise](https://github.com/marcparadise))
- Allow standalone-like configurations in plugins [\#498](https://github.com/chef/chef-server/pull/498) ([stevendanna](https://github.com/stevendanna))
- Pass eval\(\) the content of the extension, not the path to it [\#497](https://github.com/chef/chef-server/pull/497) ([stevendanna](https://github.com/stevendanna))
- Update erlectricity to resolve an encoding issue with multi-byte characters [\#496](https://github.com/chef/chef-server/pull/496) ([irvingpop](https://github.com/irvingpop))
- Update changelog for new policyfile APIs [\#495](https://github.com/chef/chef-server/pull/495) ([danielsdeleo](https://github.com/danielsdeleo))
- Document the 'granular' policy APIs [\#494](https://github.com/chef/chef-server/pull/494) ([danielsdeleo](https://github.com/danielsdeleo))
- Adjust requestor selection to reflect actual query ordering [\#493](https://github.com/chef/chef-server/pull/493) ([jcreedcmu](https://github.com/jcreedcmu))
- Allow configuration extensions to load PrivateChef config extensions [\#490](https://github.com/chef/chef-server/pull/490) ([stevendanna](https://github.com/stevendanna))
- Transition Solr memory and JVM settings from OSC11 to Chef 12. [\#488](https://github.com/chef/chef-server/pull/488) ([andy-dufour](https://github.com/andy-dufour))
- system gem load support for dvm [\#487](https://github.com/chef/chef-server/pull/487) ([marcparadise](https://github.com/marcparadise))
- chef-server-ctl external service support [\#486](https://github.com/chef/chef-server/pull/486) ([marcparadise](https://github.com/marcparadise))
- Remove yanked celluloid from Gemfile.lock [\#485](https://github.com/chef/chef-server/pull/485) ([danielsdeleo](https://github.com/danielsdeleo))
- Purge symlinks before \(re\)loading erlang projects [\#484](https://github.com/chef/chef-server/pull/484) ([danielsdeleo](https://github.com/danielsdeleo))
- Implement DELETE for Policyfile types [\#483](https://github.com/chef/chef-server/pull/483) ([danielsdeleo](https://github.com/danielsdeleo))
- Add PrivateChef\#register\_extension method for plugin configuration [\#481](https://github.com/chef/chef-server/pull/481) ([stevendanna](https://github.com/stevendanna))
- Now that luarocks is whitelisted, uncomment full matrix. [\#479](https://github.com/chef/chef-server/pull/479) ([tylercloke](https://github.com/tylercloke))
- Add production section to newrelic config [\#478](https://github.com/chef/chef-server/pull/478) ([jcreedcmu](https://github.com/jcreedcmu))
- Add basic testing for the PrivateChef configuration library [\#477](https://github.com/chef/chef-server/pull/477) ([stevendanna](https://github.com/stevendanna))
- postgres preflight check  finalization  [\#472](https://github.com/chef/chef-server/pull/472) ([marcparadise](https://github.com/marcparadise))
- postgres partybus [\#470](https://github.com/chef/chef-server/pull/470) ([joedevivo](https://github.com/joedevivo))
- Add basic contribution documentation [\#469](https://github.com/chef/chef-server/pull/469) ([stevendanna](https://github.com/stevendanna))
- Pretty CT output [\#467](https://github.com/chef/chef-server/pull/467) ([joedevivo](https://github.com/joedevivo))
- \[dvm\] add partybus loading and remove unused defaults [\#466](https://github.com/chef/chef-server/pull/466) ([marcparadise](https://github.com/marcparadise))
- By default, bind oc\_id to IPV4 address [\#465](https://github.com/chef/chef-server/pull/465) ([jtimberman](https://github.com/jtimberman))
- Add ability to pass psql options to `chef-server-ctl psql` [\#463](https://github.com/chef/chef-server/pull/463) ([jeremiahsnapp](https://github.com/jeremiahsnapp))
- \[oc\_bifrost\] Concurrency fix for update\_acl [\#462](https://github.com/chef/chef-server/pull/462) ([marcparadise](https://github.com/marcparadise))
- rebar3 [\#458](https://github.com/chef/chef-server/pull/458) ([joedevivo](https://github.com/joedevivo))
- Use the master branch of omnibus-software again. [\#457](https://github.com/chef/chef-server/pull/457) ([stevendanna](https://github.com/stevendanna))
- \[bookshelf\] Add bksw\_sync module [\#456](https://github.com/chef/chef-server/pull/456) ([stevendanna](https://github.com/stevendanna))
- Relative locations in test cases [\#455](https://github.com/chef/chef-server/pull/455) ([joedevivo](https://github.com/joedevivo))
- Renamed apps/\*/test\_utils [\#454](https://github.com/chef/chef-server/pull/454) ([joedevivo](https://github.com/joedevivo))
- Readme for chef\_objects/priv/depselector\_rb [\#453](https://github.com/chef/chef-server/pull/453) ([joedevivo](https://github.com/joedevivo))
- Removed all ../../includes from -include directives [\#452](https://github.com/chef/chef-server/pull/452) ([joedevivo](https://github.com/joedevivo))
- Get back to davisp:jiffy, 0.14.1 [\#451](https://github.com/chef/chef-server/pull/451) ([joedevivo](https://github.com/joedevivo))
- Fixed VERSIONS and dvm [\#450](https://github.com/chef/chef-server/pull/450) ([joedevivo](https://github.com/joedevivo))
- Fix bookshelf REL\_HOOK [\#449](https://github.com/chef/chef-server/pull/449) ([joedevivo](https://github.com/joedevivo))
- 12.1 the reckoning [\#448](https://github.com/chef/chef-server/pull/448) ([joedevivo](https://github.com/joedevivo))
- Make license endpoint return number of nodes in existing orgs [\#444](https://github.com/chef/chef-server/pull/444) ([jeremiahsnapp](https://github.com/jeremiahsnapp))
- \[omnibus\] modify pg\_user provider for rds security reqs [\#442](https://github.com/chef/chef-server/pull/442) ([marcparadise](https://github.com/marcparadise))
- Add features for full-installation reindex to chef-server-ctl reindex [\#439](https://github.com/chef/chef-server/pull/439) ([stevendanna](https://github.com/stevendanna))
- Bring Your Own Postgres - phase 1  [\#438](https://github.com/chef/chef-server/pull/438) ([marcparadise](https://github.com/marcparadise))
-  chef-server-ctl external postgresql sanity checks.  [\#430](https://github.com/chef/chef-server/pull/430) ([marcparadise](https://github.com/marcparadise))
- Compute version numbers for all Erlang projects [\#424](https://github.com/chef/chef-server/pull/424) ([joedevivo](https://github.com/joedevivo))
- Document the current release process for Chef Server [\#416](https://github.com/chef/chef-server/pull/416) ([stevendanna](https://github.com/stevendanna))
- Multiple API support in pedant [\#383](https://github.com/chef/chef-server/pull/383) ([andrewjamesbrown](https://github.com/andrewjamesbrown))
- chef-server-ctl backup and restore [\#174](https://github.com/chef/chef-server/pull/174) ([stevendanna](https://github.com/stevendanna))

## [12.1.2](https://github.com/chef/chef-server/tree/12.1.2) (2015-07-16)
[Full Changelog](https://github.com/chef/chef-server/compare/12.1.1...12.1.2)

**Merged pull requests:**

- Properly override chef dependency. [\#433](https://github.com/chef/chef-server/pull/433) ([tylercloke](https://github.com/tylercloke))
- Release 12.1.2. [\#432](https://github.com/chef/chef-server/pull/432) ([tylercloke](https://github.com/tylercloke))
- Use correct secrets key to prevent opscode\_chef password reset on every run [\#429](https://github.com/chef/chef-server/pull/429) ([marcparadise](https://github.com/marcparadise))
- silence oc\_id warning about config.serve\_static\_assets [\#426](https://github.com/chef/chef-server/pull/426) ([juliandunn](https://github.com/juliandunn))
- Adding psql command to chef-server-ctl [\#425](https://github.com/chef/chef-server/pull/425) ([andy-dufour](https://github.com/andy-dufour))

## [12.1.1](https://github.com/chef/chef-server/tree/12.1.1) (2015-07-13)
[Full Changelog](https://github.com/chef/chef-server/compare/12.1.0...12.1.1)

**Implemented enhancements:**

- Configurable Sign Up and Reset Password links [\#204](https://github.com/chef/chef-server/issues/204)
- Avoid Unintended Updates for RHEL [\#374](https://github.com/chef/chef-server/pull/374) ([sean-horn](https://github.com/sean-horn))

**Fixed bugs:**

- Upgrade from OSC 11.1 to Chef server 12.1 fails. [\#371](https://github.com/chef/chef-server/issues/371)

**Closed issues:**

- Addons Fail to Install on RHEL7 [\#404](https://github.com/chef/chef-server/issues/404)
- Add flexibility to allow UI to render on a non-standard port \(non 80 nor 443\) [\#358](https://github.com/chef/chef-server/issues/358)
- Install chef-server-ctl install opscode-manage 12 rc4 behind a proxy AND https transport for ubuntu [\#191](https://github.com/chef/chef-server/issues/191)
- allow full package path when installing add-ons  [\#178](https://github.com/chef/chef-server/issues/178)

**Merged pull requests:**

- Add read-only sql user for oc-id [\#420](https://github.com/chef/chef-server/pull/420) ([marcparadise](https://github.com/marcparadise))
- use tcp connections for all possible scenarios when adminstrating local db [\#418](https://github.com/chef/chef-server/pull/418) ([marcparadise](https://github.com/marcparadise))
- update private-chef-ctl so that it checks for root before proceeding [\#417](https://github.com/chef/chef-server/pull/417) ([marcparadise](https://github.com/marcparadise))
- Fix broken build by including a missed commit and correcting a bad automerge [\#414](https://github.com/chef/chef-server/pull/414) ([marcparadise](https://github.com/marcparadise))
- Update CHANGELOG for 12.1.1 release [\#413](https://github.com/chef/chef-server/pull/413) ([stevendanna](https://github.com/stevendanna))
- Bump omnibus software revision [\#411](https://github.com/chef/chef-server/pull/411) ([stevendanna](https://github.com/stevendanna))
- Bump omnibus-software revision [\#410](https://github.com/chef/chef-server/pull/410) ([stevendanna](https://github.com/stevendanna))
- Handle none case in mover\_trans\_migration\_queue:init\_queue\(\). [\#409](https://github.com/chef/chef-server/pull/409) ([tylercloke](https://github.com/tylercloke))
- Handle none case in mover\_trans\_migration\_queue:init\_queue\(\). [\#408](https://github.com/chef/chef-server/pull/408) ([tylercloke](https://github.com/tylercloke))
- Add PDF docs on the Chef server ACL system [\#407](https://github.com/chef/chef-server/pull/407) ([mmzyk](https://github.com/mmzyk))
- Adding db superuser to pg\_hba.conf [\#406](https://github.com/chef/chef-server/pull/406) ([andy-dufour](https://github.com/andy-dufour))
- add remote connection support to postgres helpers [\#405](https://github.com/chef/chef-server/pull/405) ([marcparadise](https://github.com/marcparadise))
- add secondary postgres vm option [\#402](https://github.com/chef/chef-server/pull/402) ([marcparadise](https://github.com/marcparadise))
- Include missing dependencies in mover build [\#401](https://github.com/chef/chef-server/pull/401) ([joedevivo](https://github.com/joedevivo))
- Bump knife-ec-backup to 2.0.3 to fix OSC upgrades. [\#399](https://github.com/chef/chef-server/pull/399) ([tylercloke](https://github.com/tylercloke))
- Fix option name so that --background \(and -b\) work [\#397](https://github.com/chef/chef-server/pull/397) ([jcreedcmu](https://github.com/jcreedcmu))
- pulling in changes from the byop-sqitch-provider branch [\#396](https://github.com/chef/chef-server/pull/396) ([marcparadise](https://github.com/marcparadise))
- Include missing dependencies in mover build [\#395](https://github.com/chef/chef-server/pull/395) ([stevendanna](https://github.com/stevendanna))
- Catch common error cases in global\_admins\_user\_addition migration [\#393](https://github.com/chef/chef-server/pull/393) ([stevendanna](https://github.com/stevendanna))
- Update policy docs for /policies /policy\_groups split [\#391](https://github.com/chef/chef-server/pull/391) ([danielsdeleo](https://github.com/danielsdeleo))
- Mp/dvm support custom server config [\#390](https://github.com/chef/chef-server/pull/390) ([marcparadise](https://github.com/marcparadise))
- add option for external service config recipe and create one for postgres [\#389](https://github.com/chef/chef-server/pull/389) ([marcparadise](https://github.com/marcparadise))
- Makefile and README changes [\#384](https://github.com/chef/chef-server/pull/384) ([joedevivo](https://github.com/joedevivo))
- Enable running Chefspec tests against private-chef cookbook [\#382](https://github.com/chef/chef-server/pull/382) ([charlesjohnson](https://github.com/charlesjohnson))
- Move node\['postgresql'\]\['sql\_user'\] and related to node\['opscode-erchef'\] [\#381](https://github.com/chef/chef-server/pull/381) ([marcparadise](https://github.com/marcparadise))
- ChangeLog-Entry: \[omnibus\] Make the chef postgres database be owned by the chef postgres user [\#380](https://github.com/chef/chef-server/pull/380) ([marcparadise](https://github.com/marcparadise))
- dvm omnibus load option [\#378](https://github.com/chef/chef-server/pull/378) ([marcparadise](https://github.com/marcparadise))
- build.sh\(s\) now grep relx.config for version info [\#377](https://github.com/chef/chef-server/pull/377) ([joedevivo](https://github.com/joedevivo))
- Rename ORGNAME\_global\_admins groups to ORGNAME\_read\_access\_group [\#376](https://github.com/chef/chef-server/pull/376) ([stevendanna](https://github.com/stevendanna))
- Fix bug where --expiration-date option was not properly being set in csc key commands. [\#373](https://github.com/chef/chef-server/pull/373) ([tylercloke](https://github.com/tylercloke))
- Bifrost & Mover build on Olde CI [\#372](https://github.com/chef/chef-server/pull/372) ([joedevivo](https://github.com/joedevivo))
- Update nginx.rb to use openssl cookbook [\#370](https://github.com/chef/chef-server/pull/370) ([charlesjohnson](https://github.com/charlesjohnson))
- Factored up RAML docs into common document and updated many endpoints. [\#369](https://github.com/chef/chef-server/pull/369) ([tylercloke](https://github.com/tylercloke))
- \[dvm\] do not autoload components [\#367](https://github.com/chef/chef-server/pull/367) ([marcparadise](https://github.com/marcparadise))
- Update link in oc-id README [\#362](https://github.com/chef/chef-server/pull/362) ([mmzyk](https://github.com/mmzyk))
- Bump omnibus-software for the latest libxml2 [\#355](https://github.com/chef/chef-server/pull/355) ([stevendanna](https://github.com/stevendanna))
- Make profile form functional. [\#353](https://github.com/chef/chef-server/pull/353) ([raskchanky](https://github.com/raskchanky))
- Enabled bounded queuing for erchef and bifrost squerl-based queues [\#352](https://github.com/chef/chef-server/pull/352) ([stevendanna](https://github.com/stevendanna))
- Require authentication for redis. [\#350](https://github.com/chef/chef-server/pull/350) ([stevendanna](https://github.com/stevendanna))
- Enabling external rabbitmq for analytics [\#347](https://github.com/chef/chef-server/pull/347) ([PrajaktaPurohit](https://github.com/PrajaktaPurohit))
- Adds link to oc-id blog post [\#343](https://github.com/chef/chef-server/pull/343) ([nellshamrell](https://github.com/nellshamrell))
- allow full path to the package to be specified [\#340](https://github.com/chef/chef-server/pull/340) ([alexpop](https://github.com/alexpop))
- ACL Restrictions - Prevent admins group removal from grant ACE [\#248](https://github.com/chef/chef-server/pull/248) ([mmzyk](https://github.com/mmzyk))
- Allow users to read other user objects when both users share an org [\#244](https://github.com/chef/chef-server/pull/244) ([stevendanna](https://github.com/stevendanna))

## [12.1.0](https://github.com/chef/chef-server/tree/12.1.0) (2015-06-18)
[Full Changelog](https://github.com/chef/chef-server/compare/12.1.0-rc.3...12.1.0)

**Fixed bugs:**

- postgres shared\_buffers calculation is broken because values have different units [\#44](https://github.com/chef/chef-server/issues/44)

**Closed issues:**

- ssl\_dhparam should be exposed for nginx in chef-server.rb [\#349](https://github.com/chef/chef-server/issues/349)
- Upgrade ignores port settings in chef-server.rb [\#212](https://github.com/chef/chef-server/issues/212)
- chef-server-ctl upgrade uses chef-solo [\#128](https://github.com/chef/chef-server/issues/128)
- Chef 12 'chef-server-ctl upgrade' fails on RHEL 5.11 [\#127](https://github.com/chef/chef-server/issues/127)
- oc-id login screens look weird in Chef Server 12.1.0-rc.1 [\#325](https://github.com/chef/chef-server/issues/325)

**Merged pull requests:**

- Upgrade to redis 2.8.21 for a critical security fix [\#348](https://github.com/chef/chef-server/pull/348) ([stevendanna](https://github.com/stevendanna))

## [12.1.0-rc.3](https://github.com/chef/chef-server/tree/12.1.0-rc.3) (2015-06-12)
[Full Changelog](https://github.com/chef/chef-server/compare/12.1.0-rc.2...12.1.0-rc.3)

## [12.1.0-rc.2](https://github.com/chef/chef-server/tree/12.1.0-rc.2) (2015-06-11)
[Full Changelog](https://github.com/chef/chef-server/compare/12.1.0-rc.1...12.1.0-rc.2)

**Implemented enhancements:**

- Upgrade embedded Java to Java 8 [\#109](https://github.com/chef/chef-server/issues/109)
- Unify log rotation with chef-server-ctl [\#102](https://github.com/chef/chef-server/issues/102)
- /organizations/\<orgname\>/\<anything\>/\_acl enpoint differs between EC11 & CS12 [\#15](https://github.com/chef/chef-server/issues/15)

**Fixed bugs:**

- "Could not connect to mover service" error when upgrading to 12.1.0-rc.1 [\#335](https://github.com/chef/chef-server/issues/335)
- Allow Hosted Chef Server to Refer to AWS S3 US-STANDARD Endpoint at s3.amazonaws.com [\#99](https://github.com/chef/chef-server/issues/99)
- Ubuntu 14.10: chef-server-ctl reconfigure fails if procps is already running [\#14](https://github.com/chef/chef-server/issues/14)

**Closed issues:**

- Logjam [\#333](https://github.com/chef/chef-server/issues/333)
- Need to be able to set proxy environment variables in keepalived's cluster.sh script [\#327](https://github.com/chef/chef-server/issues/327)
- Chef Server 12.0.8 -\> 12.1.0-rc.1 doesn't upgrade Solr correctly [\#323](https://github.com/chef/chef-server/issues/323)
- “add-user-key argument error: wrong number of arguments” [\#302](https://github.com/chef/chef-server/issues/302)
- 'knife node create' and node that already exist [\#263](https://github.com/chef/chef-server/issues/263)
- Custom postgresql port is ignored [\#190](https://github.com/chef/chef-server/issues/190)
- Do not allow "system" groups to be deleted [\#155](https://github.com/chef/chef-server/issues/155)
- Please update Nginx to the current release \(1.6.2\) to address security issues [\#58](https://github.com/chef/chef-server/issues/58)
- Chef 12 Pedant Failure On First Run [\#34](https://github.com/chef/chef-server/issues/34)

**Merged pull requests:**

- \(ci-95\) Redact password from actions payload. [\#346](https://github.com/chef/chef-server/pull/346) ([mcquin](https://github.com/mcquin))
- update CHANGELOG and RELEASE\_NOTES to mention the new UI [\#344](https://github.com/chef/chef-server/pull/344) ([raskchanky](https://github.com/raskchanky))
- Notify user during dhparam creation [\#338](https://github.com/chef/chef-server/pull/338) ([stevendanna](https://github.com/stevendanna))
- Policy List API [\#337](https://github.com/chef/chef-server/pull/337) ([danielsdeleo](https://github.com/danielsdeleo))
- Handle non-empty policy\_groups in policy\_groups tests [\#334](https://github.com/chef/chef-server/pull/334) ([danielsdeleo](https://github.com/danielsdeleo))
- Bump omnibus-software and omnibus to get openresty build fixes [\#330](https://github.com/chef/chef-server/pull/330) ([stevendanna](https://github.com/stevendanna))
- Use custom context configuration for solr jetty [\#328](https://github.com/chef/chef-server/pull/328) ([stevendanna](https://github.com/stevendanna))
- erlang 17.4 travis support [\#326](https://github.com/chef/chef-server/pull/326) ([marcparadise](https://github.com/marcparadise))
- Bypasses a memory leak in Erlang's ssl\_session\_cache [\#320](https://github.com/chef/chef-server/pull/320) ([joedevivo](https://github.com/joedevivo))
- Update navigation, sign-in form and layouts for VI-85 [\#319](https://github.com/chef/chef-server/pull/319) ([cnunciato](https://github.com/cnunciato))
- travis: build on erlang R16B03-1 [\#317](https://github.com/chef/chef-server/pull/317) ([sdelano](https://github.com/sdelano))
- omnibus: build chef last [\#316](https://github.com/chef/chef-server/pull/316) ([sdelano](https://github.com/sdelano))
- mover run service needs 'foreground' argument [\#311](https://github.com/chef/chef-server/pull/311) ([joedevivo](https://github.com/joedevivo))
- relx.config [\#308](https://github.com/chef/chef-server/pull/308) ([joedevivo](https://github.com/joedevivo))
- All kinds of mover action [\#305](https://github.com/chef/chef-server/pull/305) ([joedevivo](https://github.com/joedevivo))
- \[wip\] return 500 when an unexpected error occurs during authn [\#304](https://github.com/chef/chef-server/pull/304) ([sdelano](https://github.com/sdelano))

## [12.1.0-rc.1](https://github.com/chef/chef-server/tree/12.1.0-rc.1) (2015-05-28)
[Full Changelog](https://github.com/chef/chef-server/compare/12.1.0-alpha.1...12.1.0-rc.1)

**Implemented enhancements:**

- Disable org creation for users [\#88](https://github.com/chef/chef-server/issues/88)

**Fixed bugs:**

- Password reset web form identified if username doesn't exist [\#197](https://github.com/chef/chef-server/issues/197)
- Manual partybus init [\#22](https://github.com/chef/chef-server/issues/22)
- Remove nested directories from log rotation template [\#218](https://github.com/chef/chef-server/pull/218) ([ryancragun](https://github.com/ryancragun))

**Closed issues:**

- Issue in chef-server-ctl reconfigure [\#300](https://github.com/chef/chef-server/issues/300)
- \_identifiers endpoint returns 404 for a given node [\#280](https://github.com/chef/chef-server/issues/280)
- chef-server-ctl upgrade fails when a cookbook has no name metadata [\#250](https://github.com/chef/chef-server/issues/250)
- Incorrect User already exists ERROR \#2278 [\#228](https://github.com/chef/chef-server/issues/228)
- analytics should be removed as an option in chef-server-ctl install command [\#152](https://github.com/chef/chef-server/issues/152)
- Warnings on chef-server-ctl `reconfigure` [\#106](https://github.com/chef/chef-server/issues/106)
- Permissions errors when upgrading from chef-server 11 -\> 12 [\#57](https://github.com/chef/chef-server/issues/57)
- Refactor cleanup\_procs\_and\_nuke used by p-s-c/c-s-c uninstall or cleanse [\#27](https://github.com/chef/chef-server/issues/27)
- CS12rc6 upgrade of OSC 11.1.4 leaves OSC's runsvdir tree running [\#25](https://github.com/chef/chef-server/issues/25)
- Unable to remove users from Admin Group [\#24](https://github.com/chef/chef-server/issues/24)

**Merged pull requests:**

- Mp/12.1.0 changelog relnotes [\#307](https://github.com/chef/chef-server/pull/307) ([marcparadise](https://github.com/marcparadise))
- Generate 2048 bit dhparam on first reconfigure [\#301](https://github.com/chef/chef-server/pull/301) ([stevendanna](https://github.com/stevendanna))
- \[oc\_erchef\] dialyzer cleanup to remove undefined types and functions [\#296](https://github.com/chef/chef-server/pull/296) ([marcparadise](https://github.com/marcparadise))
- \[oc\_erchef\] Correct bug where incorrect module name was used for update [\#294](https://github.com/chef/chef-server/pull/294) ([marcparadise](https://github.com/marcparadise))
- \[oc\_erchef\] enable warnings\_as\_errors [\#293](https://github.com/chef/chef-server/pull/293) ([marcparadise](https://github.com/marcparadise))
- Define custom ERLANG\_DIALYZER\_APPS that includes sasl and eldap [\#291](https://github.com/chef/chef-server/pull/291) ([stevendanna](https://github.com/stevendanna))
- Cleanup remnants of the couch removal [\#290](https://github.com/chef/chef-server/pull/290) ([stevendanna](https://github.com/stevendanna))
- reindex into omnibus 2 [\#289](https://github.com/chef/chef-server/pull/289) ([marcparadise](https://github.com/marcparadise))
- Explicitly call fetch\_org\_metadata in chef\_wm\_object\_identifiers [\#287](https://github.com/chef/chef-server/pull/287) ([stevendanna](https://github.com/stevendanna))
- Erlang 17.5 Compatibility [\#285](https://github.com/chef/chef-server/pull/285) ([joedevivo](https://github.com/joedevivo))
- Tests for internal \_identifiers endpoint [\#284](https://github.com/chef/chef-server/pull/284) ([stevendanna](https://github.com/stevendanna))
- Bump omnibus-software to pull in changes to chef's build instructions [\#283](https://github.com/chef/chef-server/pull/283) ([stevendanna](https://github.com/stevendanna))
- Enable containerized travis, disable lua testing for now [\#282](https://github.com/chef/chef-server/pull/282) ([stevendanna](https://github.com/stevendanna))
- add newrelic [\#279](https://github.com/chef/chef-server/pull/279) ([raskchanky](https://github.com/raskchanky))
- Mp/couch deps and config cleanup [\#277](https://github.com/chef/chef-server/pull/277) ([marcparadise](https://github.com/marcparadise))
- \[oc\_erchef\] purge couch, couch darklaunch, and couch dependencies from erchef [\#276](https://github.com/chef/chef-server/pull/276) ([marcparadise](https://github.com/marcparadise))
- \[oc\_erchef\] move authz sql statements to pgsql\_statements.config [\#274](https://github.com/chef/chef-server/pull/274) ([marcparadise](https://github.com/marcparadise))
- \[oc\_erchef\] couch removal prep - remove chef\_cache [\#273](https://github.com/chef/chef-server/pull/273) ([marcparadise](https://github.com/marcparadise))
- \[oc\_erchef\] replace ejson calls with chef\_json [\#272](https://github.com/chef/chef-server/pull/272) ([marcparadise](https://github.com/marcparadise))
- Provide pooler bounded-queuing configuration for all pooler controlled queues [\#271](https://github.com/chef/chef-server/pull/271) ([stevendanna](https://github.com/stevendanna))
- \[oc-chef-pedant\] remove endpoint behaviors specific to ruby and pre-chef12 [\#270](https://github.com/chef/chef-server/pull/270) ([marcparadise](https://github.com/marcparadise))
- \[omnibus\] add internal attribute for sever api version in use [\#269](https://github.com/chef/chef-server/pull/269) ([marcparadise](https://github.com/marcparadise))
- Update oc\_bifrost deps to pull in the latest sqerl [\#267](https://github.com/chef/chef-server/pull/267) ([stevendanna](https://github.com/stevendanna))
- \[erchef\]\[dvm\] Add support for customizable dotfiles [\#266](https://github.com/chef/chef-server/pull/266) ([marcparadise](https://github.com/marcparadise))
- \[oc\_erchef\] update lockfile for sqerl [\#264](https://github.com/chef/chef-server/pull/264) ([marcparadise](https://github.com/marcparadise))
- Fix chef-server-ctl install bugs [\#262](https://github.com/chef/chef-server/pull/262) ([stevendanna](https://github.com/stevendanna))
- Upgrade to Postgres 9.2.10 [\#261](https://github.com/chef/chef-server/pull/261) ([sdelano](https://github.com/sdelano))
- use orgid we already have instead of looking it up again [\#258](https://github.com/chef/chef-server/pull/258) ([marcparadise](https://github.com/marcparadise))
- \[dvm\] fix for dep loading to work again in the new location [\#256](https://github.com/chef/chef-server/pull/256) ([marcparadise](https://github.com/marcparadise))
- Update the bulk fetch cookbook query to return less data [\#255](https://github.com/chef/chef-server/pull/255) ([stevendanna](https://github.com/stevendanna))
- Update folsom and bear to avoid badarith errors [\#254](https://github.com/chef/chef-server/pull/254) ([stevendanna](https://github.com/stevendanna))
- Update sqerl to latest version to address increased transaction rate [\#253](https://github.com/chef/chef-server/pull/253) ([stevendanna](https://github.com/stevendanna))
- i18nify the password resets controller [\#252](https://github.com/chef/chef-server/pull/252) ([raskchanky](https://github.com/raskchanky))
- Make the message you get when entering a bad username a little more vague [\#249](https://github.com/chef/chef-server/pull/249) ([raskchanky](https://github.com/raskchanky))
- Mp/mover policy cleanup 2 [\#246](https://github.com/chef/chef-server/pull/246) ([marcparadise](https://github.com/marcparadise))
- Fix typo in development guide [\#243](https://github.com/chef/chef-server/pull/243) ([danieldreier](https://github.com/danieldreier))
- use correct authz id, add policy cleanup migration [\#241](https://github.com/chef/chef-server/pull/241) ([marcparadise](https://github.com/marcparadise))
- Only clean depsolver bundle when clean target is run [\#238](https://github.com/chef/chef-server/pull/238) ([stevendanna](https://github.com/stevendanna))
- fix oc\_id recipe so that it doesn't try to parse log messages as json [\#235](https://github.com/chef/chef-server/pull/235) ([sdelano](https://github.com/sdelano))
- use relx tooling to not start all apps. sync versions on chef-server version [\#234](https://github.com/chef/chef-server/pull/234) ([marcparadise](https://github.com/marcparadise))
- Implemented X-Ops-Server-API-Version Response Header [\#233](https://github.com/chef/chef-server/pull/233) ([tylercloke](https://github.com/tylercloke))
- Update gather-logs script [\#231](https://github.com/chef/chef-server/pull/231) ([jeremiahsnapp](https://github.com/jeremiahsnapp))
- Cache timezone information in local file [\#230](https://github.com/chef/chef-server/pull/230) ([stevendanna](https://github.com/stevendanna))
- dvm tweaks [\#229](https://github.com/chef/chef-server/pull/229) ([mmzyk](https://github.com/mmzyk))
- Wrap `make install` with `travis\_retry` function [\#227](https://github.com/chef/chef-server/pull/227) ([stevendanna](https://github.com/stevendanna))
- Mp/dvm/coverage support [\#224](https://github.com/chef/chef-server/pull/224) ([marcparadise](https://github.com/marcparadise))
- Cache bundler, apt, cpan, and luarocks [\#223](https://github.com/chef/chef-server/pull/223) ([stevendanna](https://github.com/stevendanna))
- oc-id profile page [\#222](https://github.com/chef/chef-server/pull/222) ([smith](https://github.com/smith))
- Upgrade to Solr 4.9.1 and Jetty 8 [\#221](https://github.com/chef/chef-server/pull/221) ([stevendanna](https://github.com/stevendanna))
- \[perf\] Use list:append instead of list:flatten in chef\_s3:make\_key [\#220](https://github.com/chef/chef-server/pull/220) ([stevendanna](https://github.com/stevendanna))
- Dialyzer fixes for oc\_chef\_authz\_acl [\#219](https://github.com/chef/chef-server/pull/219) ([joedevivo](https://github.com/joedevivo))
- Repair incorrect DTD [\#217](https://github.com/chef/chef-server/pull/217) ([juliandunn](https://github.com/juliandunn))
- Dialyzer fixes in various from\_json functions [\#216](https://github.com/chef/chef-server/pull/216) ([joedevivo](https://github.com/joedevivo))
- Move the cache\_path into /var/opt/opscode to avoid warnings [\#168](https://github.com/chef/chef-server/pull/168) ([stevendanna](https://github.com/stevendanna))

## [12.1.0-alpha.1](https://github.com/chef/chef-server/tree/12.1.0-alpha.1) (2015-05-04)
[Full Changelog](https://github.com/chef/chef-server/compare/2.3.0...12.1.0-alpha.1)

**Closed issues:**

- failed to upload environment setting - FFI\_Yajl::ParseError: [\#100](https://github.com/chef/chef-server/issues/100)

**Merged pull requests:**

- remove SquareSerif font, comm-503 page, and associated resources [\#215](https://github.com/chef/chef-server/pull/215) ([smith](https://github.com/smith))
- Mp/integrated dvm improvements [\#210](https://github.com/chef/chef-server/pull/210) ([marcparadise](https://github.com/marcparadise))
- Revert "Ra/vi 75" [\#209](https://github.com/chef/chef-server/pull/209) ([smith](https://github.com/smith))
- Ra/vi 75 [\#207](https://github.com/chef/chef-server/pull/207) ([stevendanna](https://github.com/stevendanna))
- Updates to dvm to work in new repository layout [\#175](https://github.com/chef/chef-server/pull/175) ([stevendanna](https://github.com/stevendanna))
- Update README with information about the new repository layout [\#159](https://github.com/chef/chef-server/pull/159) ([stevendanna](https://github.com/stevendanna))
- \[scripts/import-components\] Import chef-server components into this repo [\#147](https://github.com/chef/chef-server/pull/147) ([stevendanna](https://github.com/stevendanna))

## [2.3.0](https://github.com/chef/chef-server/tree/2.3.0) (2015-04-30)
[Full Changelog](https://github.com/chef/chef-server/compare/1.8.3...2.3.0)

## [1.8.3](https://github.com/chef/chef-server/tree/1.8.3) (2015-04-28)
[Full Changelog](https://github.com/chef/chef-server/compare/1.8.2...1.8.3)

**Closed issues:**

- Foslom graphite causes erchef to restart over and over if graphite becomes inaccessible [\#126](https://github.com/chef/chef-server/issues/126)
- chef-mover does not build on el7 [\#117](https://github.com/chef/chef-server/issues/117)
- EC11 to CS12 upgrade uses `make` command which crashes the upgrade when not available [\#33](https://github.com/chef/chef-server/issues/33)

## [1.8.2](https://github.com/chef/chef-server/tree/1.8.2) (2015-04-27)
[Full Changelog](https://github.com/chef/chef-server/compare/0.29.2.2...1.8.2)

## [0.29.2.2](https://github.com/chef/chef-server/tree/0.29.2.2) (2015-04-24)
[Full Changelog](https://github.com/chef/chef-server/compare/0.29.2.1...0.29.2.2)

## [0.29.2.1](https://github.com/chef/chef-server/tree/0.29.2.1) (2015-04-22)
[Full Changelog](https://github.com/chef/chef-server/compare/1.8.1...0.29.2.1)

## [1.8.1](https://github.com/chef/chef-server/tree/1.8.1) (2015-04-21)
[Full Changelog](https://github.com/chef/chef-server/compare/12.0.8...1.8.1)

## [12.0.8](https://github.com/chef/chef-server/tree/12.0.8) (2015-04-20)
[Full Changelog](https://github.com/chef/chef-server/compare/11.3.1...12.0.8)

## [11.3.1](https://github.com/chef/chef-server/tree/11.3.1) (2015-04-20)
[Full Changelog](https://github.com/chef/chef-server/compare/1.8.0...11.3.1)

## [1.8.0](https://github.com/chef/chef-server/tree/1.8.0) (2015-04-17)
[Full Changelog](https://github.com/chef/chef-server/compare/2.0.5...1.8.0)

**Implemented enhancements:**

- CS12: Update Embedded Openresty Nginx [\#142](https://github.com/chef/chef-server/issues/142)

**Closed issues:**

- Bad Upgrade [\#143](https://github.com/chef/chef-server/issues/143)

## [2.0.5](https://github.com/chef/chef-server/tree/2.0.5) (2015-04-09)
[Full Changelog](https://github.com/chef/chef-server/compare/1.7.0...2.0.5)

## [1.7.0](https://github.com/chef/chef-server/tree/1.7.0) (2015-04-09)
[Full Changelog](https://github.com/chef/chef-server/compare/12.0.7...1.7.0)

**Closed issues:**

- Chef delivers old cookbook versions [\#140](https://github.com/chef/chef-server/issues/140)
- Chef tries to override non existing nodes and therefore throws a 405 Method Not Allowed [\#138](https://github.com/chef/chef-server/issues/138)

## [12.0.7](https://github.com/chef/chef-server/tree/12.0.7) (2015-03-26)
[Full Changelog](https://github.com/chef/chef-server/compare/1.6.6...12.0.7)

## [1.6.6](https://github.com/chef/chef-server/tree/1.6.6) (2015-03-25)
[Full Changelog](https://github.com/chef/chef-server/compare/2.0.4...1.6.6)

## [2.0.4](https://github.com/chef/chef-server/tree/2.0.4) (2015-03-21)
[Full Changelog](https://github.com/chef/chef-server/compare/1.6.5...2.0.4)

## [1.6.5](https://github.com/chef/chef-server/tree/1.6.5) (2015-03-21)
[Full Changelog](https://github.com/chef/chef-server/compare/12.0.6...1.6.5)

## [12.0.6](https://github.com/chef/chef-server/tree/12.0.6) (2015-03-19)
[Full Changelog](https://github.com/chef/chef-server/compare/1.6.4...12.0.6)

**Implemented enhancements:**

- Hosted Chef Should Support User Manipulation Through the API [\#77](https://github.com/chef/chef-server/issues/77)

**Merged pull requests:**

- Add ssd as core maintainer, knife-opc and knife-ec-backup lieutenant [\#93](https://github.com/chef/chef-server/pull/93) ([stevendanna](https://github.com/stevendanna))

## [1.6.4](https://github.com/chef/chef-server/tree/1.6.4) (2015-03-18)
[Full Changelog](https://github.com/chef/chef-server/compare/2.0.3...1.6.4)

## [2.0.3](https://github.com/chef/chef-server/tree/2.0.3) (2015-03-18)
[Full Changelog](https://github.com/chef/chef-server/compare/2.0.2...2.0.3)

## [2.0.2](https://github.com/chef/chef-server/tree/2.0.2) (2015-03-17)
[Full Changelog](https://github.com/chef/chef-server/compare/1.6.3...2.0.2)

## [1.6.3](https://github.com/chef/chef-server/tree/1.6.3) (2015-03-17)
[Full Changelog](https://github.com/chef/chef-server/compare/1.6.2...1.6.3)

**Implemented enhancements:**

- org-user-add -a flag does not give billing-admin rights. [\#97](https://github.com/chef/chef-server/issues/97)

**Closed issues:**

- ERROR:  relation "keys\_by\_name" does not exist [\#125](https://github.com/chef/chef-server/issues/125)
- chef-sync broken since chef-server-core 12.0.4 [\#113](https://github.com/chef/chef-server/issues/113)
- When you create a user via chef-server-ctl add-user with --filename pointed at invalid path, the user is created, but the key is not put on the filesystem. [\#17](https://github.com/chef/chef-server/issues/17)

## [1.6.2](https://github.com/chef/chef-server/tree/1.6.2) (2015-03-10)
[Full Changelog](https://github.com/chef/chef-server/compare/1.6.1...1.6.2)

**Fixed bugs:**

- LDAP users with special characters in their external\_authentication\_uid cannot log in [\#119](https://github.com/chef/chef-server/issues/119)

## [1.6.1](https://github.com/chef/chef-server/tree/1.6.1) (2015-03-07)
[Full Changelog](https://github.com/chef/chef-server/compare/2.0.1...1.6.1)

## [2.0.1](https://github.com/chef/chef-server/tree/2.0.1) (2015-03-07)
[Full Changelog](https://github.com/chef/chef-server/compare/1.6.0...2.0.1)

**Closed issues:**

- FATAL:  remaining connection slots are reserved for non-replication superuser connections [\#112](https://github.com/chef/chef-server/issues/112)
- Document Load Balancer Requirements for Chef Server [\#94](https://github.com/chef/chef-server/issues/94)

## [1.6.0](https://github.com/chef/chef-server/tree/1.6.0) (2015-02-27)
[Full Changelog](https://github.com/chef/chef-server/compare/0.5.0...1.6.0)

## [0.5.0](https://github.com/chef/chef-server/tree/0.5.0) (2015-02-26)
[Full Changelog](https://github.com/chef/chef-server/compare/12.0.5...0.5.0)

**Closed issues:**

- sudo chef-server-ctl reconfigure [\#108](https://github.com/chef/chef-server/issues/108)
- Unable to reconfigure new chef-server install - postgres not accepting connections on 5432 [\#89](https://github.com/chef/chef-server/issues/89)

## [12.0.5](https://github.com/chef/chef-server/tree/12.0.5) (2015-02-25)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.79...12.0.5)

## [1.0.79](https://github.com/chef/chef-server/tree/1.0.79) (2015-02-25)
[Full Changelog](https://github.com/chef/chef-server/compare/1.5.0...1.0.79)

**Closed issues:**

- POST support for keys API \(tracking\) [\#110](https://github.com/chef/chef-server/issues/110)

## [1.5.0](https://github.com/chef/chef-server/tree/1.5.0) (2015-02-25)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.78...1.5.0)

## [1.0.78](https://github.com/chef/chef-server/tree/1.0.78) (2015-02-25)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.77...1.0.78)

**Implemented enhancements:**

- Replication should support push/publish as well as a pull model [\#98](https://github.com/chef/chef-server/issues/98)

**Closed issues:**

- Updating a user account \(resetting a key\) breaks LDAP/AD authentication [\#66](https://github.com/chef/chef-server/issues/66)

## [1.0.77](https://github.com/chef/chef-server/tree/1.0.77) (2015-02-23)
[Full Changelog](https://github.com/chef/chef-server/compare/1.4.5...1.0.77)

## [1.4.5](https://github.com/chef/chef-server/tree/1.4.5) (2015-02-20)
[Full Changelog](https://github.com/chef/chef-server/compare/11.3.0...1.4.5)

**Closed issues:**

- failed to upload environment setting - FFI\_Yajl::ParseError: [\#101](https://github.com/chef/chef-server/issues/101)

## [11.3.0](https://github.com/chef/chef-server/tree/11.3.0) (2015-02-19)
[Full Changelog](https://github.com/chef/chef-server/compare/12.0.4...11.3.0)

**Fixed bugs:**

- CS12: LDAP Doesn't Allow Anonymous Bind [\#67](https://github.com/chef/chef-server/issues/67)

**Merged pull requests:**

- Added Tyler Cloke to the lieutenants for core server. [\#92](https://github.com/chef/chef-server/pull/92) ([tylercloke](https://github.com/tylercloke))

## [12.0.4](https://github.com/chef/chef-server/tree/12.0.4) (2015-02-18)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.76...12.0.4)

**Closed issues:**

- Upgrade from Chef 12.0.1 to 12.0.2 errors on upgrade [\#82](https://github.com/chef/chef-server/issues/82)

**Merged pull requests:**

- Add Oliver Ferrigni as a maintainer [\#95](https://github.com/chef/chef-server/pull/95) ([oferrigni](https://github.com/oferrigni))

## [1.0.76](https://github.com/chef/chef-server/tree/1.0.76) (2015-02-12)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.75...1.0.76)

**Closed issues:**

- Chef server's API auth failed then updated 12.0.2 to 12.0.3 [\#85](https://github.com/chef/chef-server/issues/85)
- "FATAL: Wrong number of arguments" running chef-server-ctl user-create if password contains shell characters that aren't escaped, even if quoted [\#84](https://github.com/chef/chef-server/issues/84)
- Chef-server 11 Installation Fails [\#78](https://github.com/chef/chef-server/issues/78)

## [1.0.75](https://github.com/chef/chef-server/tree/1.0.75) (2015-02-07)
[Full Changelog](https://github.com/chef/chef-server/compare/12.0.3...1.0.75)

**Fixed bugs:**

- CS12 LDAP Cannot Handle Duplicated "uid:" field For the Username [\#71](https://github.com/chef/chef-server/issues/71)
- CS12: LDAP Cannot Handle List Values for the "Mail:" Field [\#68](https://github.com/chef/chef-server/issues/68)
- c-s-c chef12-upgrade-download command has two -d options [\#45](https://github.com/chef/chef-server/issues/45)
- private-chef-cookbooks: is\_data\_master? Guard Unnecessary In Embedded nginx Recipe [\#37](https://github.com/chef/chef-server/issues/37)

**Closed issues:**

- chef-server-ctl returns 0 when commands fail [\#65](https://github.com/chef/chef-server/issues/65)
- $PERL5LIB breaks install [\#55](https://github.com/chef/chef-server/issues/55)

## [12.0.3](https://github.com/chef/chef-server/tree/12.0.3) (2015-02-04)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.74...12.0.3)

## [1.0.74](https://github.com/chef/chef-server/tree/1.0.74) (2015-02-03)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.73...1.0.74)

**Closed issues:**

- upgrade from 11 to 12 fails, Sequel::ForeignKeyConstraintViolation [\#73](https://github.com/chef/chef-server/issues/73)

## [1.0.73](https://github.com/chef/chef-server/tree/1.0.73) (2015-01-28)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.72...1.0.73)

## [1.0.72](https://github.com/chef/chef-server/tree/1.0.72) (2015-01-27)
[Full Changelog](https://github.com/chef/chef-server/compare/12.0.2...1.0.72)

## [12.0.2](https://github.com/chef/chef-server/tree/12.0.2) (2015-01-27)
[Full Changelog](https://github.com/chef/chef-server/compare/2.2.20...12.0.2)

## [2.2.20](https://github.com/chef/chef-server/tree/2.2.20) (2015-01-27)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.71...2.2.20)

## [1.0.71](https://github.com/chef/chef-server/tree/1.0.71) (2015-01-26)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.70...1.0.71)

**Closed issues:**

- CS12: chef server 12.0.1 upgrade to latest \(12.0.1+20150123091710.git.58.af97194-1\) postgres failure [\#70](https://github.com/chef/chef-server/issues/70)

**Merged pull requests:**

- Add a line pointing where to file issues for other projects [\#69](https://github.com/chef/chef-server/pull/69) ([mmzyk](https://github.com/mmzyk))

## [1.0.70](https://github.com/chef/chef-server/tree/1.0.70) (2015-01-21)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.69...1.0.70)

**Fixed bugs:**

- Manage 1.6.2+ Account Link Controller Depends On Next Release of erchef/Chef 12.0.x [\#41](https://github.com/chef/chef-server/issues/41)

**Closed issues:**

- upgrade from 11 to 12 fails [\#61](https://github.com/chef/chef-server/issues/61)
- Chef Server 12 does not allow client to be recrerated [\#54](https://github.com/chef/chef-server/issues/54)
- Chef Server 12.0.1 - chef-server-ctl - ffi-yajl and yajl-ruby warning [\#53](https://github.com/chef/chef-server/issues/53)
- intermittent 401s from Chef Server [\#5](https://github.com/chef/chef-server/issues/5)

**Merged pull requests:**

- Add Mark Anderson as a maintainer [\#60](https://github.com/chef/chef-server/pull/60) ([markan](https://github.com/markan))

## [1.0.69](https://github.com/chef/chef-server/tree/1.0.69) (2015-01-13)
[Full Changelog](https://github.com/chef/chef-server/compare/2.2.19...1.0.69)

**Closed issues:**

- chef-server-ctl user-create throws error if email address has a . in it [\#51](https://github.com/chef/chef-server/issues/51)
- Cookbook uploading fails when running nginx on an alternative port [\#43](https://github.com/chef/chef-server/issues/43)

**Merged pull requests:**

- Mp/maintainers doc [\#56](https://github.com/chef/chef-server/pull/56) ([marcparadise](https://github.com/marcparadise))

## [2.2.19](https://github.com/chef/chef-server/tree/2.2.19) (2014-12-17)
[Full Changelog](https://github.com/chef/chef-server/compare/11.2.6...2.2.19)

## [11.2.6](https://github.com/chef/chef-server/tree/11.2.6) (2014-12-17)
[Full Changelog](https://github.com/chef/chef-server/compare/12.0.1...11.2.6)

## [12.0.1](https://github.com/chef/chef-server/tree/12.0.1) (2014-12-17)
[Full Changelog](https://github.com/chef/chef-server/compare/0.4.4...12.0.1)

## [0.4.4](https://github.com/chef/chef-server/tree/0.4.4) (2014-12-17)
[Full Changelog](https://github.com/chef/chef-server/compare/0.30.0...0.4.4)

## [0.30.0](https://github.com/chef/chef-server/tree/0.30.0) (2014-12-09)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.68...0.30.0)

**Closed issues:**

- no resolver defined to resolve opscode\_account [\#38](https://github.com/chef/chef-server/issues/38)

## [1.0.68](https://github.com/chef/chef-server/tree/1.0.68) (2014-12-02)
[Full Changelog](https://github.com/chef/chef-server/compare/0.4.3...1.0.68)

## [0.4.3](https://github.com/chef/chef-server/tree/0.4.3) (2014-12-02)
[Full Changelog](https://github.com/chef/chef-server/compare/12.0.0...0.4.3)

## [12.0.0](https://github.com/chef/chef-server/tree/12.0.0) (2014-11-25)
[Full Changelog](https://github.com/chef/chef-server/compare/12.0.0-rc.7...12.0.0)

## [12.0.0-rc.7](https://github.com/chef/chef-server/tree/12.0.0-rc.7) (2014-11-20)
[Full Changelog](https://github.com/chef/chef-server/compare/0.29.4...12.0.0-rc.7)

## [0.29.4](https://github.com/chef/chef-server/tree/0.29.4) (2014-11-17)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.67...0.29.4)

## [1.0.67](https://github.com/chef/chef-server/tree/1.0.67) (2014-11-17)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.66...1.0.67)

## [1.0.66](https://github.com/chef/chef-server/tree/1.0.66) (2014-11-14)
[Full Changelog](https://github.com/chef/chef-server/compare/12.0.0-rc.6...1.0.66)

## [12.0.0-rc.6](https://github.com/chef/chef-server/tree/12.0.0-rc.6) (2014-11-11)
[Full Changelog](https://github.com/chef/chef-server/compare/0.29.3...12.0.0-rc.6)

## [0.29.3](https://github.com/chef/chef-server/tree/0.29.3) (2014-11-10)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.65...0.29.3)

## [1.0.65](https://github.com/chef/chef-server/tree/1.0.65) (2014-11-10)
[Full Changelog](https://github.com/chef/chef-server/compare/0.29.2...1.0.65)

## [0.29.2](https://github.com/chef/chef-server/tree/0.29.2) (2014-11-07)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.64...0.29.2)

## [1.0.64](https://github.com/chef/chef-server/tree/1.0.64) (2014-11-07)
[Full Changelog](https://github.com/chef/chef-server/compare/0.29.1...1.0.64)

**Closed issues:**

- Can't upload cookbooks after certificate update [\#16](https://github.com/chef/chef-server/issues/16)
- Attempting to disassociate an admin in an org results in a broken organization [\#13](https://github.com/chef/chef-server/issues/13)
- undefined method on chef-server-ctl password [\#12](https://github.com/chef/chef-server/issues/12)

## [0.29.1](https://github.com/chef/chef-server/tree/0.29.1) (2014-11-05)
[Full Changelog](https://github.com/chef/chef-server/compare/11.2.5...0.29.1)

**Closed issues:**

- chef-sync-ctl show-config does not work [\#10](https://github.com/chef/chef-server/issues/10)

## [11.2.5](https://github.com/chef/chef-server/tree/11.2.5) (2014-11-03)
[Full Changelog](https://github.com/chef/chef-server/compare/0.29.0...11.2.5)

**Closed issues:**

- Small typo with org-disassociate [\#11](https://github.com/chef/chef-server/issues/11)
- Small typo in chef-server-ctl [\#9](https://github.com/chef/chef-server/issues/9)
- Temp file leak [\#3](https://github.com/chef/chef-server/issues/3)

## [0.29.0](https://github.com/chef/chef-server/tree/0.29.0) (2014-10-28)
[Full Changelog](https://github.com/chef/chef-server/compare/0.28.5...0.29.0)

## [0.28.5](https://github.com/chef/chef-server/tree/0.28.5) (2014-10-28)
[Full Changelog](https://github.com/chef/chef-server/compare/0.28.4...0.28.5)

## [0.28.4](https://github.com/chef/chef-server/tree/0.28.4) (2014-10-27)
[Full Changelog](https://github.com/chef/chef-server/compare/11.2.4...0.28.4)

## [11.2.4](https://github.com/chef/chef-server/tree/11.2.4) (2014-10-27)
[Full Changelog](https://github.com/chef/chef-server/compare/0.28.3...11.2.4)

## [0.28.3](https://github.com/chef/chef-server/tree/0.28.3) (2014-10-24)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.29.5...0.28.3)

## [1.0.29.5](https://github.com/chef/chef-server/tree/1.0.29.5) (2014-10-22)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.62...1.0.29.5)

## [1.0.62](https://github.com/chef/chef-server/tree/1.0.62) (2014-10-17)
[Full Changelog](https://github.com/chef/chef-server/compare/0.28.2...1.0.62)

## [0.28.2](https://github.com/chef/chef-server/tree/0.28.2) (2014-10-17)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.61...0.28.2)

## [1.0.61](https://github.com/chef/chef-server/tree/1.0.61) (2014-10-17)
[Full Changelog](https://github.com/chef/chef-server/compare/0.28.1...1.0.61)

## [0.28.1](https://github.com/chef/chef-server/tree/0.28.1) (2014-10-17)
[Full Changelog](https://github.com/chef/chef-server/compare/2.2.17...0.28.1)

## [2.2.17](https://github.com/chef/chef-server/tree/2.2.17) (2014-10-16)
[Full Changelog](https://github.com/chef/chef-server/compare/2.2.16...2.2.17)

## [2.2.16](https://github.com/chef/chef-server/tree/2.2.16) (2014-10-16)
[Full Changelog](https://github.com/chef/chef-server/compare/11.2.3...2.2.16)

## [11.2.3](https://github.com/chef/chef-server/tree/11.2.3) (2014-10-16)
[Full Changelog](https://github.com/chef/chef-server/compare/1.4.15...11.2.3)

## [1.4.15](https://github.com/chef/chef-server/tree/1.4.15) (2014-10-16)
[Full Changelog](https://github.com/chef/chef-server/compare/12.0.0-rc.5...1.4.15)

## [12.0.0-rc.5](https://github.com/chef/chef-server/tree/12.0.0-rc.5) (2014-10-16)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.60...12.0.0-rc.5)

## [1.0.60](https://github.com/chef/chef-server/tree/1.0.60) (2014-10-15)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.29.4...1.0.60)

## [1.0.29.4](https://github.com/chef/chef-server/tree/1.0.29.4) (2014-10-15)
[Full Changelog](https://github.com/chef/chef-server/compare/0.28.0...1.0.29.4)

## [0.28.0](https://github.com/chef/chef-server/tree/0.28.0) (2014-10-14)
[Full Changelog](https://github.com/chef/chef-server/compare/0.27.7...0.28.0)

**Closed issues:**

- chef-server-ctl upgrade error: ERROR: File chef/cookbooks/var/chef is a directory while file chef/cookbooks/var/chef is a regular file   [\#2](https://github.com/chef/chef-server/issues/2)

## [0.27.7](https://github.com/chef/chef-server/tree/0.27.7) (2014-10-09)
[Full Changelog](https://github.com/chef/chef-server/compare/2.2.15...0.27.7)

**Merged pull requests:**

- added information about Chef Server subcomponents [\#1](https://github.com/chef/chef-server/pull/1) ([juliandunn](https://github.com/juliandunn))

## [2.2.15](https://github.com/chef/chef-server/tree/2.2.15) (2014-10-06)
[Full Changelog](https://github.com/chef/chef-server/compare/0.4.2...2.2.15)

## [0.4.2](https://github.com/chef/chef-server/tree/0.4.2) (2014-10-05)
[Full Changelog](https://github.com/chef/chef-server/compare/0.26.3.2...0.4.2)

## [0.26.3.2](https://github.com/chef/chef-server/tree/0.26.3.2) (2014-10-02)
[Full Changelog](https://github.com/chef/chef-server/compare/0.26.3.1...0.26.3.2)

## [0.26.3.1](https://github.com/chef/chef-server/tree/0.26.3.1) (2014-10-02)
[Full Changelog](https://github.com/chef/chef-server/compare/0.27.6...0.26.3.1)

## [0.27.6](https://github.com/chef/chef-server/tree/0.27.6) (2014-10-01)
[Full Changelog](https://github.com/chef/chef-server/compare/0.27.5...0.27.6)

## [0.27.5](https://github.com/chef/chef-server/tree/0.27.5) (2014-09-29)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.59...0.27.5)

## [1.0.59](https://github.com/chef/chef-server/tree/1.0.59) (2014-09-26)
[Full Changelog](https://github.com/chef/chef-server/compare/0.27.4...1.0.59)

## [0.27.4](https://github.com/chef/chef-server/tree/0.27.4) (2014-09-25)
[Full Changelog](https://github.com/chef/chef-server/compare/1.1.3.1...0.27.4)

## [1.1.3.1](https://github.com/chef/chef-server/tree/1.1.3.1) (2014-09-25)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.14.2...1.1.3.1)

## [0.25.14.2](https://github.com/chef/chef-server/tree/0.25.14.2) (2014-09-23)
[Full Changelog](https://github.com/chef/chef-server/compare/0.27.3...0.25.14.2)

## [0.27.3](https://github.com/chef/chef-server/tree/0.27.3) (2014-09-22)
[Full Changelog](https://github.com/chef/chef-server/compare/1.4.14...0.27.3)

## [1.4.14](https://github.com/chef/chef-server/tree/1.4.14) (2014-09-18)
[Full Changelog](https://github.com/chef/chef-server/compare/11.2.2...1.4.14)

## [11.2.2](https://github.com/chef/chef-server/tree/11.2.2) (2014-09-18)
[Full Changelog](https://github.com/chef/chef-server/compare/12.0.0-rc.4...11.2.2)

## [12.0.0-rc.4](https://github.com/chef/chef-server/tree/12.0.0-rc.4) (2014-09-18)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.58...12.0.0-rc.4)

## [1.0.58](https://github.com/chef/chef-server/tree/1.0.58) (2014-09-16)
[Full Changelog](https://github.com/chef/chef-server/compare/12.0.0-rc.3...1.0.58)

## [12.0.0-rc.3](https://github.com/chef/chef-server/tree/12.0.0-rc.3) (2014-09-10)
[Full Changelog](https://github.com/chef/chef-server/compare/12.0.0-rc.2...12.0.0-rc.3)

## [12.0.0-rc.2](https://github.com/chef/chef-server/tree/12.0.0-rc.2) (2014-09-08)
[Full Changelog](https://github.com/chef/chef-server/compare/12.0.0-rc.1...12.0.0-rc.2)

## [12.0.0-rc.1](https://github.com/chef/chef-server/tree/12.0.0-rc.1) (2014-09-07)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.57...12.0.0-rc.1)

## [1.0.57](https://github.com/chef/chef-server/tree/1.0.57) (2014-09-07)
[Full Changelog](https://github.com/chef/chef-server/compare/0.27.2...1.0.57)

## [0.27.2](https://github.com/chef/chef-server/tree/0.27.2) (2014-09-06)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.56...0.27.2)

## [1.0.56](https://github.com/chef/chef-server/tree/1.0.56) (2014-09-05)
[Full Changelog](https://github.com/chef/chef-server/compare/0.27.1...1.0.56)

## [0.27.1](https://github.com/chef/chef-server/tree/0.27.1) (2014-09-05)
[Full Changelog](https://github.com/chef/chef-server/compare/2.2.14...0.27.1)

## [2.2.14](https://github.com/chef/chef-server/tree/2.2.14) (2014-09-04)
[Full Changelog](https://github.com/chef/chef-server/compare/11.2.1...2.2.14)

## [11.2.1](https://github.com/chef/chef-server/tree/11.2.1) (2014-09-03)
[Full Changelog](https://github.com/chef/chef-server/compare/0.26.8...11.2.1)

## [0.26.8](https://github.com/chef/chef-server/tree/0.26.8) (2014-09-02)
[Full Changelog](https://github.com/chef/chef-server/compare/0.26.7...0.26.8)

## [0.26.7](https://github.com/chef/chef-server/tree/0.26.7) (2014-08-30)
[Full Changelog](https://github.com/chef/chef-server/compare/0.26.6...0.26.7)

## [0.26.6](https://github.com/chef/chef-server/tree/0.26.6) (2014-08-30)
[Full Changelog](https://github.com/chef/chef-server/compare/11.2.99...0.26.6)

## [11.2.99](https://github.com/chef/chef-server/tree/11.2.99) (2014-08-30)
[Full Changelog](https://github.com/chef/chef-server/compare/0.26.5...11.2.99)

## [0.26.5](https://github.com/chef/chef-server/tree/0.26.5) (2014-08-30)
[Full Changelog](https://github.com/chef/chef-server/compare/2.2.13...0.26.5)

## [2.2.13](https://github.com/chef/chef-server/tree/2.2.13) (2014-08-29)
[Full Changelog](https://github.com/chef/chef-server/compare/0.26.4...2.2.13)

## [0.26.4](https://github.com/chef/chef-server/tree/0.26.4) (2014-08-29)
[Full Changelog](https://github.com/chef/chef-server/compare/2.2.12...0.26.4)

## [2.2.12](https://github.com/chef/chef-server/tree/2.2.12) (2014-08-29)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.55...2.2.12)

## [1.0.55](https://github.com/chef/chef-server/tree/1.0.55) (2014-08-28)
[Full Changelog](https://github.com/chef/chef-server/compare/11.2.0-rc.4...1.0.55)

## [11.2.0-rc.4](https://github.com/chef/chef-server/tree/11.2.0-rc.4) (2014-08-28)
[Full Changelog](https://github.com/chef/chef-server/compare/11.2.0...11.2.0-rc.4)

## [11.2.0](https://github.com/chef/chef-server/tree/11.2.0) (2014-08-28)
[Full Changelog](https://github.com/chef/chef-server/compare/11.2.0-rc.3...11.2.0)

## [11.2.0-rc.3](https://github.com/chef/chef-server/tree/11.2.0-rc.3) (2014-08-27)
[Full Changelog](https://github.com/chef/chef-server/compare/11.2.0-rc.2...11.2.0-rc.3)

## [11.2.0-rc.2](https://github.com/chef/chef-server/tree/11.2.0-rc.2) (2014-08-26)
[Full Changelog](https://github.com/chef/chef-server/compare/2.2.11...11.2.0-rc.2)

## [2.2.11](https://github.com/chef/chef-server/tree/2.2.11) (2014-08-25)
[Full Changelog](https://github.com/chef/chef-server/compare/11.2.0-rc.1...2.2.11)

## [11.2.0-rc.1](https://github.com/chef/chef-server/tree/11.2.0-rc.1) (2014-08-25)
[Full Changelog](https://github.com/chef/chef-server/compare/11.2.0-rc.0...11.2.0-rc.1)

## [11.2.0-rc.0](https://github.com/chef/chef-server/tree/11.2.0-rc.0) (2014-08-25)
[Full Changelog](https://github.com/chef/chef-server/compare/2.2.10...11.2.0-rc.0)

## [2.2.10](https://github.com/chef/chef-server/tree/2.2.10) (2014-08-20)
[Full Changelog](https://github.com/chef/chef-server/compare/0.26.3...2.2.10)

## [0.26.3](https://github.com/chef/chef-server/tree/0.26.3) (2014-08-19)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.54...0.26.3)

## [1.0.54](https://github.com/chef/chef-server/tree/1.0.54) (2014-08-18)
[Full Changelog](https://github.com/chef/chef-server/compare/0.26.2...1.0.54)

## [0.26.2](https://github.com/chef/chef-server/tree/0.26.2) (2014-08-18)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.14.1...0.26.2)

## [0.25.14.1](https://github.com/chef/chef-server/tree/0.25.14.1) (2014-08-15)
[Full Changelog](https://github.com/chef/chef-server/compare/0.26.1...0.25.14.1)

## [0.26.1](https://github.com/chef/chef-server/tree/0.26.1) (2014-08-14)
[Full Changelog](https://github.com/chef/chef-server/compare/0.26.0...0.26.1)

## [0.26.0](https://github.com/chef/chef-server/tree/0.26.0) (2014-08-13)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.53...0.26.0)

## [1.0.53](https://github.com/chef/chef-server/tree/1.0.53) (2014-08-08)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.29.3...1.0.53)

## [1.0.29.3](https://github.com/chef/chef-server/tree/1.0.29.3) (2014-08-07)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.52...1.0.29.3)

## [1.0.52](https://github.com/chef/chef-server/tree/1.0.52) (2014-08-07)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.51...1.0.52)

## [1.0.51](https://github.com/chef/chef-server/tree/1.0.51) (2014-07-31)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.23...1.0.51)

## [0.25.23](https://github.com/chef/chef-server/tree/0.25.23) (2014-07-31)
[Full Changelog](https://github.com/chef/chef-server/compare/0.4.0...0.25.23)

## [0.4.0](https://github.com/chef/chef-server/tree/0.4.0) (2014-07-31)
[Full Changelog](https://github.com/chef/chef-server/compare/2.2.9...0.4.0)

## [2.2.9](https://github.com/chef/chef-server/tree/2.2.9) (2014-07-30)
[Full Changelog](https://github.com/chef/chef-server/compare/2.2.8...2.2.9)

## [2.2.8](https://github.com/chef/chef-server/tree/2.2.8) (2014-07-28)
[Full Changelog](https://github.com/chef/chef-server/compare/11.1.99...2.2.8)

## [11.1.99](https://github.com/chef/chef-server/tree/11.1.99) (2014-07-28)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.50...11.1.99)

## [1.0.50](https://github.com/chef/chef-server/tree/1.0.50) (2014-07-25)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.22...1.0.50)

## [0.25.22](https://github.com/chef/chef-server/tree/0.25.22) (2014-07-25)
[Full Changelog](https://github.com/chef/chef-server/compare/2.2.7...0.25.22)

## [2.2.7](https://github.com/chef/chef-server/tree/2.2.7) (2014-07-22)
[Full Changelog](https://github.com/chef/chef-server/compare/2.2.6...2.2.7)

## [2.2.6](https://github.com/chef/chef-server/tree/2.2.6) (2014-07-22)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.49...2.2.6)

## [1.0.49](https://github.com/chef/chef-server/tree/1.0.49) (2014-07-21)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.21...1.0.49)

## [0.25.21](https://github.com/chef/chef-server/tree/0.25.21) (2014-07-18)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.20...0.25.21)

## [0.25.20](https://github.com/chef/chef-server/tree/0.25.20) (2014-07-16)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.48...0.25.20)

## [1.0.48](https://github.com/chef/chef-server/tree/1.0.48) (2014-07-15)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.47.1...1.0.48)

## [1.0.47.1](https://github.com/chef/chef-server/tree/1.0.47.1) (2014-07-15)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.29.2...1.0.47.1)

## [1.0.29.2](https://github.com/chef/chef-server/tree/1.0.29.2) (2014-07-15)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.19...1.0.29.2)

## [0.25.19](https://github.com/chef/chef-server/tree/0.25.19) (2014-07-14)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.47...0.25.19)

## [1.0.47](https://github.com/chef/chef-server/tree/1.0.47) (2014-07-11)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.18...1.0.47)

## [0.25.18](https://github.com/chef/chef-server/tree/0.25.18) (2014-07-11)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.46...0.25.18)

## [1.0.46](https://github.com/chef/chef-server/tree/1.0.46) (2014-07-10)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.17...1.0.46)

## [0.25.17](https://github.com/chef/chef-server/tree/0.25.17) (2014-07-09)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.45...0.25.17)

## [1.0.45](https://github.com/chef/chef-server/tree/1.0.45) (2014-07-09)
[Full Changelog](https://github.com/chef/chef-server/compare/2.2.5...1.0.45)

## [2.2.5](https://github.com/chef/chef-server/tree/2.2.5) (2014-07-08)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.16...2.2.5)

## [0.25.16](https://github.com/chef/chef-server/tree/0.25.16) (2014-07-08)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.44...0.25.16)

## [1.0.44](https://github.com/chef/chef-server/tree/1.0.44) (2014-07-07)
[Full Changelog](https://github.com/chef/chef-server/compare/0.3.3...1.0.44)

## [0.3.3](https://github.com/chef/chef-server/tree/0.3.3) (2014-07-07)
[Full Changelog](https://github.com/chef/chef-server/compare/0.3.2...0.3.3)

## [0.3.2](https://github.com/chef/chef-server/tree/0.3.2) (2014-07-04)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.15...0.3.2)

## [0.25.15](https://github.com/chef/chef-server/tree/0.25.15) (2014-07-03)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.14...0.25.15)

## [0.25.14](https://github.com/chef/chef-server/tree/0.25.14) (2014-07-03)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.13...0.25.14)

## [0.25.13](https://github.com/chef/chef-server/tree/0.25.13) (2014-07-02)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.12...0.25.13)

## [0.25.12](https://github.com/chef/chef-server/tree/0.25.12) (2014-07-02)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.43...0.25.12)

## [1.0.43](https://github.com/chef/chef-server/tree/1.0.43) (2014-07-01)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.11...1.0.43)

## [0.25.11](https://github.com/chef/chef-server/tree/0.25.11) (2014-06-30)
[Full Changelog](https://github.com/chef/chef-server/compare/0.3.1...0.25.11)

## [0.3.1](https://github.com/chef/chef-server/tree/0.3.1) (2014-06-29)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.29.1...0.3.1)

## [1.0.29.1](https://github.com/chef/chef-server/tree/1.0.29.1) (2014-06-26)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.10...1.0.29.1)

## [0.25.10](https://github.com/chef/chef-server/tree/0.25.10) (2014-06-26)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.9...0.25.10)

## [0.25.9](https://github.com/chef/chef-server/tree/0.25.9) (2014-06-26)
[Full Changelog](https://github.com/chef/chef-server/compare/11.1.8...0.25.9)

## [11.1.8](https://github.com/chef/chef-server/tree/11.1.8) (2014-06-26)
[Full Changelog](https://github.com/chef/chef-server/compare/1.4.13...11.1.8)

## [1.4.13](https://github.com/chef/chef-server/tree/1.4.13) (2014-06-25)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.8...1.4.13)

## [0.25.8](https://github.com/chef/chef-server/tree/0.25.8) (2014-06-25)
[Full Changelog](https://github.com/chef/chef-server/compare/11.1.8-rc.1...0.25.8)

## [11.1.8-rc.1](https://github.com/chef/chef-server/tree/11.1.8-rc.1) (2014-06-25)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.7...11.1.8-rc.1)

## [0.25.7](https://github.com/chef/chef-server/tree/0.25.7) (2014-06-25)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.6...0.25.7)

## [0.25.6](https://github.com/chef/chef-server/tree/0.25.6) (2014-06-23)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.42...0.25.6)

## [1.0.42](https://github.com/chef/chef-server/tree/1.0.42) (2014-06-20)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.41...1.0.42)

## [1.0.41](https://github.com/chef/chef-server/tree/1.0.41) (2014-06-20)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.40...1.0.41)

## [1.0.40](https://github.com/chef/chef-server/tree/1.0.40) (2014-06-20)
[Full Changelog](https://github.com/chef/chef-server/compare/11.1.7...1.0.40)

## [11.1.7](https://github.com/chef/chef-server/tree/11.1.7) (2014-06-19)
[Full Changelog](https://github.com/chef/chef-server/compare/1.4.13-rc.1...11.1.7)

## [1.4.13-rc.1](https://github.com/chef/chef-server/tree/1.4.13-rc.1) (2014-06-19)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.39...1.4.13-rc.1)

## [1.0.39](https://github.com/chef/chef-server/tree/1.0.39) (2014-06-19)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.5...1.0.39)

## [0.25.5](https://github.com/chef/chef-server/tree/0.25.5) (2014-06-19)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.38...0.25.5)

## [1.0.38](https://github.com/chef/chef-server/tree/1.0.38) (2014-06-19)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.4...1.0.38)

## [0.25.4](https://github.com/chef/chef-server/tree/0.25.4) (2014-06-19)
[Full Changelog](https://github.com/chef/chef-server/compare/1.4.12...0.25.4)

## [1.4.12](https://github.com/chef/chef-server/tree/1.4.12) (2014-06-19)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.37...1.4.12)

## [1.0.37](https://github.com/chef/chef-server/tree/1.0.37) (2014-06-17)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.36...1.0.37)

## [1.0.36](https://github.com/chef/chef-server/tree/1.0.36) (2014-06-14)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.35...1.0.36)

## [1.0.35](https://github.com/chef/chef-server/tree/1.0.35) (2014-06-11)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.34...1.0.35)

## [1.0.34](https://github.com/chef/chef-server/tree/1.0.34) (2014-06-10)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.3...1.0.34)

## [0.25.3](https://github.com/chef/chef-server/tree/0.25.3) (2014-06-06)
[Full Changelog](https://github.com/chef/chef-server/compare/11.1.6...0.25.3)

## [11.1.6](https://github.com/chef/chef-server/tree/11.1.6) (2014-06-06)
[Full Changelog](https://github.com/chef/chef-server/compare/1.4.11...11.1.6)

## [1.4.11](https://github.com/chef/chef-server/tree/1.4.11) (2014-06-05)
[Full Changelog](https://github.com/chef/chef-server/compare/0.3.0...1.4.11)

## [0.3.0](https://github.com/chef/chef-server/tree/0.3.0) (2014-06-05)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.2...0.3.0)

## [0.25.2](https://github.com/chef/chef-server/tree/0.25.2) (2014-06-05)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.33...0.25.2)

## [1.0.33](https://github.com/chef/chef-server/tree/1.0.33) (2014-06-05)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.1...1.0.33)

## [0.25.1](https://github.com/chef/chef-server/tree/0.25.1) (2014-06-03)
[Full Changelog](https://github.com/chef/chef-server/compare/2.2.4...0.25.1)

## [2.2.4](https://github.com/chef/chef-server/tree/2.2.4) (2014-05-30)
[Full Changelog](https://github.com/chef/chef-server/compare/0.25.0...2.2.4)

## [0.25.0](https://github.com/chef/chef-server/tree/0.25.0) (2014-05-23)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.30...0.25.0)

## [1.0.30](https://github.com/chef/chef-server/tree/1.0.30) (2014-05-23)
[Full Changelog](https://github.com/chef/chef-server/compare/11.1.5...1.0.30)

## [11.1.5](https://github.com/chef/chef-server/tree/11.1.5) (2014-05-16)
[Full Changelog](https://github.com/chef/chef-server/compare/0.24.6...11.1.5)

## [0.24.6](https://github.com/chef/chef-server/tree/0.24.6) (2014-05-15)
[Full Changelog](https://github.com/chef/chef-server/compare/0.2.0...0.24.6)

## [0.2.0](https://github.com/chef/chef-server/tree/0.2.0) (2014-05-14)
[Full Changelog](https://github.com/chef/chef-server/compare/2.2.3...0.2.0)

## [2.2.3](https://github.com/chef/chef-server/tree/2.2.3) (2014-05-12)
[Full Changelog](https://github.com/chef/chef-server/compare/2.2.2...2.2.3)

## [2.2.2](https://github.com/chef/chef-server/tree/2.2.2) (2014-05-09)
[Full Changelog](https://github.com/chef/chef-server/compare/11.1.4...2.2.2)

## [11.1.4](https://github.com/chef/chef-server/tree/11.1.4) (2014-05-07)
[Full Changelog](https://github.com/chef/chef-server/compare/1.4.10...11.1.4)

## [1.4.10](https://github.com/chef/chef-server/tree/1.4.10) (2014-05-07)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.29...1.4.10)

## [1.0.29](https://github.com/chef/chef-server/tree/1.0.29) (2014-05-06)
[Full Changelog](https://github.com/chef/chef-server/compare/0.24.5...1.0.29)

## [0.24.5](https://github.com/chef/chef-server/tree/0.24.5) (2014-05-01)
[Full Changelog](https://github.com/chef/chef-server/compare/2.2.1...0.24.5)

## [2.2.1](https://github.com/chef/chef-server/tree/2.2.1) (2014-04-23)
[Full Changelog](https://github.com/chef/chef-server/compare/0.1.2...2.2.1)

## [0.1.2](https://github.com/chef/chef-server/tree/0.1.2) (2014-04-21)
[Full Changelog](https://github.com/chef/chef-server/compare/0.24.4...0.1.2)

## [0.24.4](https://github.com/chef/chef-server/tree/0.24.4) (2014-04-16)
[Full Changelog](https://github.com/chef/chef-server/compare/0.24.3...0.24.4)

## [0.24.3](https://github.com/chef/chef-server/tree/0.24.3) (2014-04-11)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.28...0.24.3)

## [1.0.28](https://github.com/chef/chef-server/tree/1.0.28) (2014-04-10)
[Full Changelog](https://github.com/chef/chef-server/compare/1.4.9...1.0.28)

## [1.4.9](https://github.com/chef/chef-server/tree/1.4.9) (2014-04-09)
[Full Changelog](https://github.com/chef/chef-server/compare/11.1.3...1.4.9)

## [11.1.3](https://github.com/chef/chef-server/tree/11.1.3) (2014-04-09)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.27...11.1.3)

## [1.0.27](https://github.com/chef/chef-server/tree/1.0.27) (2014-04-07)
[Full Changelog](https://github.com/chef/chef-server/compare/0.24.2...1.0.27)

## [0.24.2](https://github.com/chef/chef-server/tree/0.24.2) (2014-04-02)
[Full Changelog](https://github.com/chef/chef-server/compare/2.2.0...0.24.2)

## [2.2.0](https://github.com/chef/chef-server/tree/2.2.0) (2014-04-01)
[Full Changelog](https://github.com/chef/chef-server/compare/0.1.1...2.2.0)

## [0.1.1](https://github.com/chef/chef-server/tree/0.1.1) (2014-04-01)
[Full Changelog](https://github.com/chef/chef-server/compare/2.1.7...0.1.1)

## [2.1.7](https://github.com/chef/chef-server/tree/2.1.7) (2014-03-18)
[Full Changelog](https://github.com/chef/chef-server/compare/2.1.6...2.1.7)

## [2.1.6](https://github.com/chef/chef-server/tree/2.1.6) (2014-03-13)
[Full Changelog](https://github.com/chef/chef-server/compare/2.1.5...2.1.6)

## [2.1.5](https://github.com/chef/chef-server/tree/2.1.5) (2014-03-13)
[Full Changelog](https://github.com/chef/chef-server/compare/2.1.4...2.1.5)

## [2.1.4](https://github.com/chef/chef-server/tree/2.1.4) (2014-03-06)
[Full Changelog](https://github.com/chef/chef-server/compare/0.24.1...2.1.4)

## [0.24.1](https://github.com/chef/chef-server/tree/0.24.1) (2014-03-04)
[Full Changelog](https://github.com/chef/chef-server/compare/0.24.0...0.24.1)

## [0.24.0](https://github.com/chef/chef-server/tree/0.24.0) (2014-03-04)
[Full Changelog](https://github.com/chef/chef-server/compare/2.1.3...0.24.0)

## [2.1.3](https://github.com/chef/chef-server/tree/2.1.3) (2014-03-03)
[Full Changelog](https://github.com/chef/chef-server/compare/1.4.8...2.1.3)

## [1.4.8](https://github.com/chef/chef-server/tree/1.4.8) (2014-02-28)
[Full Changelog](https://github.com/chef/chef-server/compare/11.1.2...1.4.8)

## [11.1.2](https://github.com/chef/chef-server/tree/11.1.2) (2014-02-27)
[Full Changelog](https://github.com/chef/chef-server/compare/2.1.2...11.1.2)

## [2.1.2](https://github.com/chef/chef-server/tree/2.1.2) (2014-02-24)
[Full Changelog](https://github.com/chef/chef-server/compare/0.23.3...2.1.2)

## [0.23.3](https://github.com/chef/chef-server/tree/0.23.3) (2014-02-21)
[Full Changelog](https://github.com/chef/chef-server/compare/11.1.1...0.23.3)

## [11.1.1](https://github.com/chef/chef-server/tree/11.1.1) (2014-02-18)
[Full Changelog](https://github.com/chef/chef-server/compare/1.4.7...11.1.1)

## [1.4.7](https://github.com/chef/chef-server/tree/1.4.7) (2014-02-18)
[Full Changelog](https://github.com/chef/chef-server/compare/11.1.0...1.4.7)

## [11.1.0](https://github.com/chef/chef-server/tree/11.1.0) (2014-02-10)
[Full Changelog](https://github.com/chef/chef-server/compare/2.1.1...11.1.0)

## [2.1.1](https://github.com/chef/chef-server/tree/2.1.1) (2014-02-06)
[Full Changelog](https://github.com/chef/chef-server/compare/11.1.0-rc.2...2.1.1)

## [11.1.0-rc.2](https://github.com/chef/chef-server/tree/11.1.0-rc.2) (2014-02-05)
[Full Changelog](https://github.com/chef/chef-server/compare/11.1.0-rc.1...11.1.0-rc.2)

## [11.1.0-rc.1](https://github.com/chef/chef-server/tree/11.1.0-rc.1) (2014-01-29)
[Full Changelog](https://github.com/chef/chef-server/compare/0.23.2...11.1.0-rc.1)

## [0.23.2](https://github.com/chef/chef-server/tree/0.23.2) (2014-01-23)
[Full Changelog](https://github.com/chef/chef-server/compare/0.23.1...0.23.2)

## [0.23.1](https://github.com/chef/chef-server/tree/0.23.1) (2014-01-23)
[Full Changelog](https://github.com/chef/chef-server/compare/11.1.0-beta.3...0.23.1)

## [11.1.0-beta.3](https://github.com/chef/chef-server/tree/11.1.0-beta.3) (2014-01-16)
[Full Changelog](https://github.com/chef/chef-server/compare/11.1.0-beta.2...11.1.0-beta.3)

## [11.1.0-beta.2](https://github.com/chef/chef-server/tree/11.1.0-beta.2) (2014-01-16)
[Full Changelog](https://github.com/chef/chef-server/compare/0.23.0...11.1.0-beta.2)

## [0.23.0](https://github.com/chef/chef-server/tree/0.23.0) (2014-01-16)
[Full Changelog](https://github.com/chef/chef-server/compare/2.1.0...0.23.0)

## [2.1.0](https://github.com/chef/chef-server/tree/2.1.0) (2014-01-15)
[Full Changelog](https://github.com/chef/chef-server/compare/11.1.0-beta.1...2.1.0)

## [11.1.0-beta.1](https://github.com/chef/chef-server/tree/11.1.0-beta.1) (2014-01-15)
[Full Changelog](https://github.com/chef/chef-server/compare/0.22.2...11.1.0-beta.1)

## [0.22.2](https://github.com/chef/chef-server/tree/0.22.2) (2014-01-14)
[Full Changelog](https://github.com/chef/chef-server/compare/11.1.0-alpha.0...0.22.2)

## [11.1.0-alpha.0](https://github.com/chef/chef-server/tree/11.1.0-alpha.0) (2014-01-10)
[Full Changelog](https://github.com/chef/chef-server/compare/0.22.1...11.1.0-alpha.0)

## [0.22.1](https://github.com/chef/chef-server/tree/0.22.1) (2013-12-17)
[Full Changelog](https://github.com/chef/chef-server/compare/11.0.2-ned.7...0.22.1)

## [11.0.2-ned.7](https://github.com/chef/chef-server/tree/11.0.2-ned.7) (2013-12-16)
[Full Changelog](https://github.com/chef/chef-server/compare/11.0.2-ned.6...11.0.2-ned.7)

## [11.0.2-ned.6](https://github.com/chef/chef-server/tree/11.0.2-ned.6) (2013-12-15)
[Full Changelog](https://github.com/chef/chef-server/compare/1.4.1...11.0.2-ned.6)

## [1.4.1](https://github.com/chef/chef-server/tree/1.4.1) (2013-12-12)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.37...1.4.1)

## [0.21.37](https://github.com/chef/chef-server/tree/0.21.37) (2013-12-11)
[Full Changelog](https://github.com/chef/chef-server/compare/11.0.2-ned.5...0.21.37)

## [11.0.2-ned.5](https://github.com/chef/chef-server/tree/11.0.2-ned.5) (2013-12-05)
[Full Changelog](https://github.com/chef/chef-server/compare/11.0.2-ned.4...11.0.2-ned.5)

## [11.0.2-ned.4](https://github.com/chef/chef-server/tree/11.0.2-ned.4) (2013-12-04)
[Full Changelog](https://github.com/chef/chef-server/compare/11.0.2...11.0.2-ned.4)

## [11.0.2](https://github.com/chef/chef-server/tree/11.0.2) (2013-12-04)
[Full Changelog](https://github.com/chef/chef-server/compare/11.0.2-ned.3...11.0.2)

## [11.0.2-ned.3](https://github.com/chef/chef-server/tree/11.0.2-ned.3) (2013-12-03)
[Full Changelog](https://github.com/chef/chef-server/compare/11.0.2-ned.2...11.0.2-ned.3)

## [11.0.2-ned.2](https://github.com/chef/chef-server/tree/11.0.2-ned.2) (2013-12-02)
[Full Changelog](https://github.com/chef/chef-server/compare/11.0.2-rc.4...11.0.2-ned.2)

## [11.0.2-rc.4](https://github.com/chef/chef-server/tree/11.0.2-rc.4) (2013-11-27)
[Full Changelog](https://github.com/chef/chef-server/compare/11.0.2-ned.1...11.0.2-rc.4)

## [11.0.2-ned.1](https://github.com/chef/chef-server/tree/11.0.2-ned.1) (2013-11-27)
[Full Changelog](https://github.com/chef/chef-server/compare/11.0.2-rc.3...11.0.2-ned.1)

## [11.0.2-rc.3](https://github.com/chef/chef-server/tree/11.0.2-rc.3) (2013-11-26)
[Full Changelog](https://github.com/chef/chef-server/compare/11.0.2-rc.2...11.0.2-rc.3)

## [11.0.2-rc.2](https://github.com/chef/chef-server/tree/11.0.2-rc.2) (2013-11-22)
[Full Changelog](https://github.com/chef/chef-server/compare/11.0.2-rc.1...11.0.2-rc.2)

## [11.0.2-rc.1](https://github.com/chef/chef-server/tree/11.0.2-rc.1) (2013-11-19)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.3...11.0.2-rc.1)

## [1.0.3](https://github.com/chef/chef-server/tree/1.0.3) (2013-11-19)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.36...1.0.3)

## [0.21.36](https://github.com/chef/chef-server/tree/0.21.36) (2013-11-04)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.35...0.21.36)

## [0.21.35](https://github.com/chef/chef-server/tree/0.21.35) (2013-11-01)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.34...0.21.35)

## [0.21.34](https://github.com/chef/chef-server/tree/0.21.34) (2013-10-31)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.33...0.21.34)

## [0.21.33](https://github.com/chef/chef-server/tree/0.21.33) (2013-10-25)
[Full Changelog](https://github.com/chef/chef-server/compare/11.0.1...0.21.33)

## [11.0.1](https://github.com/chef/chef-server/tree/11.0.1) (2013-10-25)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.32...11.0.1)

## [0.21.32](https://github.com/chef/chef-server/tree/0.21.32) (2013-10-25)
[Full Changelog](https://github.com/chef/chef-server/compare/2.0.0...0.21.32)

## [2.0.0](https://github.com/chef/chef-server/tree/2.0.0) (2013-10-16)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.31...2.0.0)

## [0.21.31](https://github.com/chef/chef-server/tree/0.21.31) (2013-10-15)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.30...0.21.31)

## [0.21.30](https://github.com/chef/chef-server/tree/0.21.30) (2013-10-11)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.29...0.21.30)

## [0.21.29](https://github.com/chef/chef-server/tree/0.21.29) (2013-10-04)
[Full Changelog](https://github.com/chef/chef-server/compare/11.0.0...0.21.29)

## [11.0.0](https://github.com/chef/chef-server/tree/11.0.0) (2013-09-30)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.28...11.0.0)

## [0.21.28](https://github.com/chef/chef-server/tree/0.21.28) (2013-09-25)
[Full Changelog](https://github.com/chef/chef-server/compare/11.0.0-rc.3...0.21.28)

## [11.0.0-rc.3](https://github.com/chef/chef-server/tree/11.0.0-rc.3) (2013-09-20)
[Full Changelog](https://github.com/chef/chef-server/compare/11.0.0-rc.2...11.0.0-rc.3)

## [11.0.0-rc.2](https://github.com/chef/chef-server/tree/11.0.0-rc.2) (2013-09-19)
[Full Changelog](https://github.com/chef/chef-server/compare/11.0.0-rc.1...11.0.0-rc.2)

## [11.0.0-rc.1](https://github.com/chef/chef-server/tree/11.0.0-rc.1) (2013-09-17)
[Full Changelog](https://github.com/chef/chef-server/compare/11.0.0-tech.preview.4...11.0.0-rc.1)

## [11.0.0-tech.preview.4](https://github.com/chef/chef-server/tree/11.0.0-tech.preview.4) (2013-09-17)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.27...11.0.0-tech.preview.4)

## [0.21.27](https://github.com/chef/chef-server/tree/0.21.27) (2013-09-16)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.2...0.21.27)

## [1.0.2](https://github.com/chef/chef-server/tree/1.0.2) (2013-09-16)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.1...1.0.2)

## [1.0.1](https://github.com/chef/chef-server/tree/1.0.1) (2013-09-15)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.0...1.0.1)

## [1.0.0](https://github.com/chef/chef-server/tree/1.0.0) (2013-09-13)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.26...1.0.0)

## [0.21.26](https://github.com/chef/chef-server/tree/0.21.26) (2013-09-05)
[Full Changelog](https://github.com/chef/chef-server/compare/0.2.8...0.21.26)

## [0.2.8](https://github.com/chef/chef-server/tree/0.2.8) (2013-09-04)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.25...0.2.8)

## [0.21.25](https://github.com/chef/chef-server/tree/0.21.25) (2013-08-28)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.24...0.21.25)

## [0.21.24](https://github.com/chef/chef-server/tree/0.21.24) (2013-08-27)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.23...0.21.24)

## [0.21.23](https://github.com/chef/chef-server/tree/0.21.23) (2013-08-23)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.22...0.21.23)

## [0.21.22](https://github.com/chef/chef-server/tree/0.21.22) (2013-08-22)
[Full Changelog](https://github.com/chef/chef-server/compare/0.2.7...0.21.22)

## [0.2.7](https://github.com/chef/chef-server/tree/0.2.7) (2013-08-20)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.21...0.2.7)

## [0.21.21](https://github.com/chef/chef-server/tree/0.21.21) (2013-08-19)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.20...0.21.21)

## [0.21.20](https://github.com/chef/chef-server/tree/0.21.20) (2013-08-16)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.19...0.21.20)

## [0.21.19](https://github.com/chef/chef-server/tree/0.21.19) (2013-08-15)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.18...0.21.19)

## [0.21.18](https://github.com/chef/chef-server/tree/0.21.18) (2013-08-13)
[Full Changelog](https://github.com/chef/chef-server/compare/11.0.0-tech.preview.3...0.21.18)

## [11.0.0-tech.preview.3](https://github.com/chef/chef-server/tree/11.0.0-tech.preview.3) (2013-08-13)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.17...11.0.0-tech.preview.3)

## [0.21.17](https://github.com/chef/chef-server/tree/0.21.17) (2013-08-12)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.16...0.21.17)

## [0.21.16](https://github.com/chef/chef-server/tree/0.21.16) (2013-08-09)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.15...0.21.16)

## [0.21.15](https://github.com/chef/chef-server/tree/0.21.15) (2013-08-07)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.14...0.21.15)

## [0.21.14](https://github.com/chef/chef-server/tree/0.21.14) (2013-08-06)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.13...0.21.14)

## [0.21.13](https://github.com/chef/chef-server/tree/0.21.13) (2013-08-03)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.12...0.21.13)

## [0.21.12](https://github.com/chef/chef-server/tree/0.21.12) (2013-08-02)
[Full Changelog](https://github.com/chef/chef-server/compare/0.2.6...0.21.12)

## [0.2.6](https://github.com/chef/chef-server/tree/0.2.6) (2013-07-31)
[Full Changelog](https://github.com/chef/chef-server/compare/0.2.5...0.2.6)

## [0.2.5](https://github.com/chef/chef-server/tree/0.2.5) (2013-07-26)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.11...0.2.5)

## [0.21.11](https://github.com/chef/chef-server/tree/0.21.11) (2013-07-25)
[Full Changelog](https://github.com/chef/chef-server/compare/1.3.1...0.21.11)

## [1.3.1](https://github.com/chef/chef-server/tree/1.3.1) (2013-07-24)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.10...1.3.1)

## [0.21.10](https://github.com/chef/chef-server/tree/0.21.10) (2013-07-18)
[Full Changelog](https://github.com/chef/chef-server/compare/request-logger...0.21.10)

## [request-logger](https://github.com/chef/chef-server/tree/request-logger) (2013-07-18)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.26...request-logger)

## [1.0.26](https://github.com/chef/chef-server/tree/1.0.26) (2013-07-16)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.25...1.0.26)

## [1.0.25](https://github.com/chef/chef-server/tree/1.0.25) (2013-07-12)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.24...1.0.25)

## [1.0.24](https://github.com/chef/chef-server/tree/1.0.24) (2013-07-10)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.9...1.0.24)

## [0.21.9](https://github.com/chef/chef-server/tree/0.21.9) (2013-07-10)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.23...0.21.9)

## [1.0.23](https://github.com/chef/chef-server/tree/1.0.23) (2013-07-03)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.22...1.0.23)

## [1.0.22](https://github.com/chef/chef-server/tree/1.0.22) (2013-07-01)
[Full Changelog](https://github.com/chef/chef-server/compare/1.3.0...1.0.22)

## [1.3.0](https://github.com/chef/chef-server/tree/1.3.0) (2013-06-28)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.8...1.3.0)

## [0.21.8](https://github.com/chef/chef-server/tree/0.21.8) (2013-06-28)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.7...0.21.8)

## [0.21.7](https://github.com/chef/chef-server/tree/0.21.7) (2013-06-25)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.6...0.21.7)

## [0.21.6](https://github.com/chef/chef-server/tree/0.21.6) (2013-06-25)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.5...0.21.6)

## [0.21.5](https://github.com/chef/chef-server/tree/0.21.5) (2013-06-24)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.21...0.21.5)

## [1.0.21](https://github.com/chef/chef-server/tree/1.0.21) (2013-06-24)
[Full Changelog](https://github.com/chef/chef-server/compare/11.0.0-tech.preview.2...1.0.21)

## [11.0.0-tech.preview.2](https://github.com/chef/chef-server/tree/11.0.0-tech.preview.2) (2013-06-24)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.4...11.0.0-tech.preview.2)

## [0.21.4](https://github.com/chef/chef-server/tree/0.21.4) (2013-06-23)
[Full Changelog](https://github.com/chef/chef-server/compare/11.0.0-tech.preview.1...0.21.4)

## [11.0.0-tech.preview.1](https://github.com/chef/chef-server/tree/11.0.0-tech.preview.1) (2013-06-21)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.20...11.0.0-tech.preview.1)

## [1.0.20](https://github.com/chef/chef-server/tree/1.0.20) (2013-06-19)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.3...1.0.20)

## [0.21.3](https://github.com/chef/chef-server/tree/0.21.3) (2013-06-18)
[Full Changelog](https://github.com/chef/chef-server/compare/1.4.7-ned.rc.2...0.21.3)

## [1.4.7-ned.rc.2](https://github.com/chef/chef-server/tree/1.4.7-ned.rc.2) (2013-06-18)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.2...1.4.7-ned.rc.2)

## [0.21.2](https://github.com/chef/chef-server/tree/0.21.2) (2013-06-17)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.19...0.21.2)

## [1.0.19](https://github.com/chef/chef-server/tree/1.0.19) (2013-06-17)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.1...1.0.19)

## [0.21.1](https://github.com/chef/chef-server/tree/0.21.1) (2013-06-14)
[Full Changelog](https://github.com/chef/chef-server/compare/0.21.0...0.21.1)

## [0.21.0](https://github.com/chef/chef-server/tree/0.21.0) (2013-06-14)
[Full Changelog](https://github.com/chef/chef-server/compare/0.20.6...0.21.0)

## [0.20.6](https://github.com/chef/chef-server/tree/0.20.6) (2013-06-12)
[Full Changelog](https://github.com/chef/chef-server/compare/0.20.5...0.20.6)

## [0.20.5](https://github.com/chef/chef-server/tree/0.20.5) (2013-06-12)
[Full Changelog](https://github.com/chef/chef-server/compare/0.20.4...0.20.5)

## [0.20.4](https://github.com/chef/chef-server/tree/0.20.4) (2013-06-07)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.18...0.20.4)

## [1.0.18](https://github.com/chef/chef-server/tree/1.0.18) (2013-06-06)
[Full Changelog](https://github.com/chef/chef-server/compare/0.20.3...1.0.18)

## [0.20.3](https://github.com/chef/chef-server/tree/0.20.3) (2013-05-30)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.17...0.20.3)

## [1.0.17](https://github.com/chef/chef-server/tree/1.0.17) (2013-05-28)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.16...1.0.17)

## [1.0.16](https://github.com/chef/chef-server/tree/1.0.16) (2013-05-25)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.11...1.0.16)

## [1.0.11](https://github.com/chef/chef-server/tree/1.0.11) (2013-05-24)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.15...1.0.11)

## [1.0.15](https://github.com/chef/chef-server/tree/1.0.15) (2013-05-24)
[Full Changelog](https://github.com/chef/chef-server/compare/0.20.2...1.0.15)

## [0.20.2](https://github.com/chef/chef-server/tree/0.20.2) (2013-05-24)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.14...0.20.2)

## [1.0.14](https://github.com/chef/chef-server/tree/1.0.14) (2013-05-23)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.13...1.0.14)

## [1.0.13](https://github.com/chef/chef-server/tree/1.0.13) (2013-05-22)
[Full Changelog](https://github.com/chef/chef-server/compare/0.20.1...1.0.13)

## [0.20.1](https://github.com/chef/chef-server/tree/0.20.1) (2013-05-22)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.10...0.20.1)

## [1.0.10](https://github.com/chef/chef-server/tree/1.0.10) (2013-05-22)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.12...1.0.10)

## [1.0.12](https://github.com/chef/chef-server/tree/1.0.12) (2013-05-17)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.9...1.0.12)

## [1.0.9](https://github.com/chef/chef-server/tree/1.0.9) (2013-05-17)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.8...1.0.9)

## [1.0.8](https://github.com/chef/chef-server/tree/1.0.8) (2013-05-17)
[Full Changelog](https://github.com/chef/chef-server/compare/0.20.0...1.0.8)

## [0.20.0](https://github.com/chef/chef-server/tree/0.20.0) (2013-05-16)
[Full Changelog](https://github.com/chef/chef-server/compare/0.19.12...0.20.0)

## [0.19.12](https://github.com/chef/chef-server/tree/0.19.12) (2013-05-09)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.7...0.19.12)

## [1.0.7](https://github.com/chef/chef-server/tree/1.0.7) (2013-05-08)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.6...1.0.7)

## [1.0.6](https://github.com/chef/chef-server/tree/1.0.6) (2013-05-08)
[Full Changelog](https://github.com/chef/chef-server/compare/dev-1.0.5...1.0.6)

## [dev-1.0.5](https://github.com/chef/chef-server/tree/dev-1.0.5) (2013-05-07)
[Full Changelog](https://github.com/chef/chef-server/compare/dev-1.0.4...dev-1.0.5)

## [dev-1.0.4](https://github.com/chef/chef-server/tree/dev-1.0.4) (2013-05-05)
[Full Changelog](https://github.com/chef/chef-server/compare/0.19.11...dev-1.0.4)

## [0.19.11](https://github.com/chef/chef-server/tree/0.19.11) (2013-05-01)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.5...0.19.11)

## [1.0.5](https://github.com/chef/chef-server/tree/1.0.5) (2013-04-30)
[Full Changelog](https://github.com/chef/chef-server/compare/dev-1.0.3...1.0.5)

## [dev-1.0.3](https://github.com/chef/chef-server/tree/dev-1.0.3) (2013-04-26)
[Full Changelog](https://github.com/chef/chef-server/compare/0.19.10...dev-1.0.3)

## [0.19.10](https://github.com/chef/chef-server/tree/0.19.10) (2013-04-25)
[Full Changelog](https://github.com/chef/chef-server/compare/1.0.4...0.19.10)

## [1.0.4](https://github.com/chef/chef-server/tree/1.0.4) (2013-04-12)
[Full Changelog](https://github.com/chef/chef-server/compare/0.19.9...1.0.4)

## [0.19.9](https://github.com/chef/chef-server/tree/0.19.9) (2013-04-12)
[Full Changelog](https://github.com/chef/chef-server/compare/0.2.2...0.19.9)

## [0.2.2](https://github.com/chef/chef-server/tree/0.2.2) (2013-04-12)
[Full Changelog](https://github.com/chef/chef-server/compare/0.19.8...0.2.2)

## [0.19.8](https://github.com/chef/chef-server/tree/0.19.8) (2013-04-08)
[Full Changelog](https://github.com/chef/chef-server/compare/dev-1.0.2...0.19.8)

## [dev-1.0.2](https://github.com/chef/chef-server/tree/dev-1.0.2) (2013-04-05)
[Full Changelog](https://github.com/chef/chef-server/compare/1.4.6...dev-1.0.2)

## [1.4.6](https://github.com/chef/chef-server/tree/1.4.6) (2013-03-02)
[Full Changelog](https://github.com/chef/chef-server/compare/1.4.6-rc.3...1.4.6)

## [1.4.6-rc.3](https://github.com/chef/chef-server/tree/1.4.6-rc.3) (2013-03-02)
[Full Changelog](https://github.com/chef/chef-server/compare/1.4.6-rc.2...1.4.6-rc.3)

## [1.4.6-rc.2](https://github.com/chef/chef-server/tree/1.4.6-rc.2) (2013-03-01)
[Full Changelog](https://github.com/chef/chef-server/compare/1.4.6-rc.1...1.4.6-rc.2)

## [1.4.6-rc.1](https://github.com/chef/chef-server/tree/1.4.6-rc.1) (2013-02-26)
[Full Changelog](https://github.com/chef/chef-server/compare/1.4.6-rc.0...1.4.6-rc.1)

## [1.4.6-rc.0](https://github.com/chef/chef-server/tree/1.4.6-rc.0) (2013-02-23)
[Full Changelog](https://github.com/chef/chef-server/compare/dev-1.0.0...1.4.6-rc.0)

## [dev-1.0.0](https://github.com/chef/chef-server/tree/dev-1.0.0) (2013-02-22)
[Full Changelog](https://github.com/chef/chef-server/compare/0.19.7...dev-1.0.0)

## [0.19.7](https://github.com/chef/chef-server/tree/0.19.7) (2013-02-08)
[Full Changelog](https://github.com/chef/chef-server/compare/1.4.6-alpha.0...0.19.7)

## [1.4.6-alpha.0](https://github.com/chef/chef-server/tree/1.4.6-alpha.0) (2013-02-06)
[Full Changelog](https://github.com/chef/chef-server/compare/0.19.6...1.4.6-alpha.0)

## [0.19.6](https://github.com/chef/chef-server/tree/0.19.6) (2013-02-06)
[Full Changelog](https://github.com/chef/chef-server/compare/0.19.5...0.19.6)

## [0.19.5](https://github.com/chef/chef-server/tree/0.19.5) (2013-02-01)
[Full Changelog](https://github.com/chef/chef-server/compare/0.2.1...0.19.5)

## [0.2.1](https://github.com/chef/chef-server/tree/0.2.1) (2013-02-01)
[Full Changelog](https://github.com/chef/chef-server/compare/0.19.4...0.2.1)

## [0.19.4](https://github.com/chef/chef-server/tree/0.19.4) (2013-01-30)
[Full Changelog](https://github.com/chef/chef-server/compare/0.19.3...0.19.4)

## [0.19.3](https://github.com/chef/chef-server/tree/0.19.3) (2013-01-30)
[Full Changelog](https://github.com/chef/chef-server/compare/0.19.2...0.19.3)

## [0.19.2](https://github.com/chef/chef-server/tree/0.19.2) (2013-01-30)
[Full Changelog](https://github.com/chef/chef-server/compare/0.19.1...0.19.2)

## [0.19.1](https://github.com/chef/chef-server/tree/0.19.1) (2013-01-30)
[Full Changelog](https://github.com/chef/chef-server/compare/0.19.0...0.19.1)

## [0.19.0](https://github.com/chef/chef-server/tree/0.19.0) (2013-01-29)
[Full Changelog](https://github.com/chef/chef-server/compare/0.18.2...0.19.0)

## [0.18.2](https://github.com/chef/chef-server/tree/0.18.2) (2013-01-24)
[Full Changelog](https://github.com/chef/chef-server/compare/2.0.0-alpha.0.uncle.ned.20130122.0...0.18.2)

## [2.0.0-alpha.0.uncle.ned.20130122.0](https://github.com/chef/chef-server/tree/2.0.0-alpha.0.uncle.ned.20130122.0) (2013-01-23)
[Full Changelog](https://github.com/chef/chef-server/compare/rel-1.0.2...2.0.0-alpha.0.uncle.ned.20130122.0)

## [rel-1.0.2](https://github.com/chef/chef-server/tree/rel-1.0.2) (2013-01-22)
[Full Changelog](https://github.com/chef/chef-server/compare/0.18.1...rel-1.0.2)

## [0.18.1](https://github.com/chef/chef-server/tree/0.18.1) (2013-01-22)
[Full Changelog](https://github.com/chef/chef-server/compare/1.4.4...0.18.1)

## [1.4.4](https://github.com/chef/chef-server/tree/1.4.4) (2013-01-20)
[Full Changelog](https://github.com/chef/chef-server/compare/rel-1.0.1...1.4.4)

## [rel-1.0.1](https://github.com/chef/chef-server/tree/rel-1.0.1) (2013-01-20)
[Full Changelog](https://github.com/chef/chef-server/compare/1.4.2...rel-1.0.1)

## [1.4.2](https://github.com/chef/chef-server/tree/1.4.2) (2013-01-18)
[Full Changelog](https://github.com/chef/chef-server/compare/1.4.0...1.4.2)

## [1.4.0](https://github.com/chef/chef-server/tree/1.4.0) (2013-01-17)
[Full Changelog](https://github.com/chef/chef-server/compare/rel-0.18.0...1.4.0)

## [rel-0.18.0](https://github.com/chef/chef-server/tree/rel-0.18.0) (2013-01-16)
[Full Changelog](https://github.com/chef/chef-server/compare/rel-0.1.1...rel-0.18.0)

## [rel-0.1.1](https://github.com/chef/chef-server/tree/rel-0.1.1) (2013-01-15)
[Full Changelog](https://github.com/chef/chef-server/compare/2.0.0-alpha.0.uncle.ned.20130110.0...rel-0.1.1)

## [2.0.0-alpha.0.uncle.ned.20130110.0](https://github.com/chef/chef-server/tree/2.0.0-alpha.0.uncle.ned.20130110.0) (2013-01-11)
[Full Changelog](https://github.com/chef/chef-server/compare/1.2.8.3...2.0.0-alpha.0.uncle.ned.20130110.0)

## [1.2.8.3](https://github.com/chef/chef-server/tree/1.2.8.3) (2013-01-11)
[Full Changelog](https://github.com/chef/chef-server/compare/rel-1.0.0...1.2.8.3)

## [rel-1.0.0](https://github.com/chef/chef-server/tree/rel-1.0.0) (2013-01-11)
[Full Changelog](https://github.com/chef/chef-server/compare/1.2.8.2...rel-1.0.0)

## [1.2.8.2](https://github.com/chef/chef-server/tree/1.2.8.2) (2012-10-25)
[Full Changelog](https://github.com/chef/chef-server/compare/rel-0.15.4...1.2.8.2)

## [rel-0.15.4](https://github.com/chef/chef-server/tree/rel-0.15.4) (2012-10-11)
[Full Changelog](https://github.com/chef/chef-server/compare/rel-0.15.3...rel-0.15.4)

## [rel-0.15.3](https://github.com/chef/chef-server/tree/rel-0.15.3) (2012-10-10)
[Full Changelog](https://github.com/chef/chef-server/compare/uncle-ned-8...rel-0.15.3)

## [uncle-ned-8](https://github.com/chef/chef-server/tree/uncle-ned-8) (2012-10-03)
[Full Changelog](https://github.com/chef/chef-server/compare/uncle-ned-7...uncle-ned-8)

## [uncle-ned-7](https://github.com/chef/chef-server/tree/uncle-ned-7) (2012-10-03)
[Full Changelog](https://github.com/chef/chef-server/compare/uncle-ned-6...uncle-ned-7)

## [uncle-ned-6](https://github.com/chef/chef-server/tree/uncle-ned-6) (2012-10-02)
[Full Changelog](https://github.com/chef/chef-server/compare/uncle-ned-5...uncle-ned-6)

## [uncle-ned-5](https://github.com/chef/chef-server/tree/uncle-ned-5) (2012-10-02)
[Full Changelog](https://github.com/chef/chef-server/compare/uncle-ned-4...uncle-ned-5)

## [uncle-ned-4](https://github.com/chef/chef-server/tree/uncle-ned-4) (2012-10-01)
[Full Changelog](https://github.com/chef/chef-server/compare/uncle-ned-3...uncle-ned-4)

## [uncle-ned-3](https://github.com/chef/chef-server/tree/uncle-ned-3) (2012-10-01)
[Full Changelog](https://github.com/chef/chef-server/compare/ned-1.0...uncle-ned-3)

## [ned-1.0](https://github.com/chef/chef-server/tree/ned-1.0) (2012-09-29)
[Full Changelog](https://github.com/chef/chef-server/compare/uncle-ned-2...ned-1.0)

## [uncle-ned-2](https://github.com/chef/chef-server/tree/uncle-ned-2) (2012-09-29)
[Full Changelog](https://github.com/chef/chef-server/compare/uncle-ned-1...uncle-ned-2)

## [uncle-ned-1](https://github.com/chef/chef-server/tree/uncle-ned-1) (2012-09-29)
[Full Changelog](https://github.com/chef/chef-server/compare/uncle-ned...uncle-ned-1)

## [uncle-ned](https://github.com/chef/chef-server/tree/uncle-ned) (2012-09-29)
[Full Changelog](https://github.com/chef/chef-server/compare/pc-rel-0.15.2...uncle-ned)

## [pc-rel-0.15.2](https://github.com/chef/chef-server/tree/pc-rel-0.15.2) (2012-09-21)
[Full Changelog](https://github.com/chef/chef-server/compare/rel-0.15.2...pc-rel-0.15.2)

## [rel-0.15.2](https://github.com/chef/chef-server/tree/rel-0.15.2) (2012-09-21)
[Full Changelog](https://github.com/chef/chef-server/compare/pc-rel-0.15.1...rel-0.15.2)

## [pc-rel-0.15.1](https://github.com/chef/chef-server/tree/pc-rel-0.15.1) (2012-09-20)
[Full Changelog](https://github.com/chef/chef-server/compare/rel-0.15.1...pc-rel-0.15.1)

## [rel-0.15.1](https://github.com/chef/chef-server/tree/rel-0.15.1) (2012-09-20)
[Full Changelog](https://github.com/chef/chef-server/compare/pc-rel-0.15.0...rel-0.15.1)

## [pc-rel-0.15.0](https://github.com/chef/chef-server/tree/pc-rel-0.15.0) (2012-09-20)
[Full Changelog](https://github.com/chef/chef-server/compare/rel-0.15.0...pc-rel-0.15.0)

## [rel-0.15.0](https://github.com/chef/chef-server/tree/rel-0.15.0) (2012-09-20)
[Full Changelog](https://github.com/chef/chef-server/compare/1.2.8.1...rel-0.15.0)

## [1.2.8.1](https://github.com/chef/chef-server/tree/1.2.8.1) (2012-09-18)
[Full Changelog](https://github.com/chef/chef-server/compare/rel-0.1.0...1.2.8.1)

## [rel-0.1.0](https://github.com/chef/chef-server/tree/rel-0.1.0) (2012-09-10)
[Full Changelog](https://github.com/chef/chef-server/compare/1.2.8...rel-0.1.0)

## [1.2.8](https://github.com/chef/chef-server/tree/1.2.8) (2012-08-31)
[Full Changelog](https://github.com/chef/chef-server/compare/pc-rel-0.14.0...1.2.8)

## [pc-rel-0.14.0](https://github.com/chef/chef-server/tree/pc-rel-0.14.0) (2012-08-28)
[Full Changelog](https://github.com/chef/chef-server/compare/rel-0.14.0...pc-rel-0.14.0)

## [rel-0.14.0](https://github.com/chef/chef-server/tree/rel-0.14.0) (2012-08-27)
[Full Changelog](https://github.com/chef/chef-server/compare/1.2.7...rel-0.14.0)

## [1.2.7](https://github.com/chef/chef-server/tree/1.2.7) (2012-07-18)
[Full Changelog](https://github.com/chef/chef-server/compare/1.2.6...1.2.7)

## [1.2.6](https://github.com/chef/chef-server/tree/1.2.6) (2012-07-18)
[Full Changelog](https://github.com/chef/chef-server/compare/1.2.4...1.2.6)

## [1.2.4](https://github.com/chef/chef-server/tree/1.2.4) (2012-06-22)
[Full Changelog](https://github.com/chef/chef-server/compare/1.2.2...1.2.4)

## [1.2.2](https://github.com/chef/chef-server/tree/1.2.2) (2012-06-18)
[Full Changelog](https://github.com/chef/chef-server/compare/1.2.3...1.2.2)

## [1.2.3](https://github.com/chef/chef-server/tree/1.2.3) (2012-06-18)
[Full Changelog](https://github.com/chef/chef-server/compare/1.2.1...1.2.3)

## [1.2.1](https://github.com/chef/chef-server/tree/1.2.1) (2012-06-18)
[Full Changelog](https://github.com/chef/chef-server/compare/1.2.0...1.2.1)

## [1.2.0](https://github.com/chef/chef-server/tree/1.2.0) (2012-06-18)
[Full Changelog](https://github.com/chef/chef-server/compare/1.1.19...1.2.0)

## [1.1.19](https://github.com/chef/chef-server/tree/1.1.19) (2012-04-17)
[Full Changelog](https://github.com/chef/chef-server/compare/1.1.18...1.1.19)

## [1.1.18](https://github.com/chef/chef-server/tree/1.1.18) (2012-04-13)
[Full Changelog](https://github.com/chef/chef-server/compare/1.1.16...1.1.18)

## [1.1.16](https://github.com/chef/chef-server/tree/1.1.16) (2012-04-12)
[Full Changelog](https://github.com/chef/chef-server/compare/1.1.14...1.1.16)

## [1.1.14](https://github.com/chef/chef-server/tree/1.1.14) (2012-04-11)
[Full Changelog](https://github.com/chef/chef-server/compare/1.1.12...1.1.14)

## [1.1.12](https://github.com/chef/chef-server/tree/1.1.12) (2012-04-11)
[Full Changelog](https://github.com/chef/chef-server/compare/1.1.10...1.1.12)

## [1.1.10](https://github.com/chef/chef-server/tree/1.1.10) (2012-04-11)
[Full Changelog](https://github.com/chef/chef-server/compare/1.1.8.1...1.1.10)

## [1.1.8.1](https://github.com/chef/chef-server/tree/1.1.8.1) (2012-03-29)
[Full Changelog](https://github.com/chef/chef-server/compare/1.1.8...1.1.8.1)

## [1.1.8](https://github.com/chef/chef-server/tree/1.1.8) (2012-03-23)
[Full Changelog](https://github.com/chef/chef-server/compare/1.1.9...1.1.8)

## [1.1.9](https://github.com/chef/chef-server/tree/1.1.9) (2012-03-23)
[Full Changelog](https://github.com/chef/chef-server/compare/1.1.6...1.1.9)

## [1.1.6](https://github.com/chef/chef-server/tree/1.1.6) (2012-03-22)
[Full Changelog](https://github.com/chef/chef-server/compare/1.1.7...1.1.6)

## [1.1.7](https://github.com/chef/chef-server/tree/1.1.7) (2012-03-22)
[Full Changelog](https://github.com/chef/chef-server/compare/1.1.4...1.1.7)

## [1.1.4](https://github.com/chef/chef-server/tree/1.1.4) (2012-03-22)
[Full Changelog](https://github.com/chef/chef-server/compare/1.1.5...1.1.4)

## [1.1.5](https://github.com/chef/chef-server/tree/1.1.5) (2012-03-22)
[Full Changelog](https://github.com/chef/chef-server/compare/1.1.2...1.1.5)

## [1.1.2](https://github.com/chef/chef-server/tree/1.1.2) (2012-03-19)
[Full Changelog](https://github.com/chef/chef-server/compare/1.1.3...1.1.2)

## [1.1.3](https://github.com/chef/chef-server/tree/1.1.3) (2012-03-19)
[Full Changelog](https://github.com/chef/chef-server/compare/1.1.0...1.1.3)

## [1.1.0](https://github.com/chef/chef-server/tree/1.1.0) (2012-03-16)
[Full Changelog](https://github.com/chef/chef-server/compare/rel-1.1.0...1.1.0)

## [rel-1.1.0](https://github.com/chef/chef-server/tree/rel-1.1.0) (2012-03-16)
[Full Changelog](https://github.com/chef/chef-server/compare/1.1.1...rel-1.1.0)

## [1.1.1](https://github.com/chef/chef-server/tree/1.1.1) (2012-03-16)
[Full Changelog](https://github.com/chef/chef-server/compare/0.0.9...1.1.1)

## [0.0.9](https://github.com/chef/chef-server/tree/0.0.9) (2012-03-06)
[Full Changelog](https://github.com/chef/chef-server/compare/0.0.8...0.0.9)

## [0.0.8](https://github.com/chef/chef-server/tree/0.0.8) (2012-03-06)
[Full Changelog](https://github.com/chef/chef-server/compare/0.0.7...0.0.8)

## [0.0.7](https://github.com/chef/chef-server/tree/0.0.7) (2012-03-05)
[Full Changelog](https://github.com/chef/chef-server/compare/0.0.6...0.0.7)

## [0.0.6](https://github.com/chef/chef-server/tree/0.0.6) (2012-03-05)
[Full Changelog](https://github.com/chef/chef-server/compare/0.0.5...0.0.6)

## [0.0.5](https://github.com/chef/chef-server/tree/0.0.5) (2012-03-05)
[Full Changelog](https://github.com/chef/chef-server/compare/0.0.4...0.0.5)

## [0.0.4](https://github.com/chef/chef-server/tree/0.0.4) (2012-03-05)
[Full Changelog](https://github.com/chef/chef-server/compare/0.0.3...0.0.4)

## [0.0.3](https://github.com/chef/chef-server/tree/0.0.3) (2012-03-05)
[Full Changelog](https://github.com/chef/chef-server/compare/0.0.2...0.0.3)

## [0.0.2](https://github.com/chef/chef-server/tree/0.0.2) (2012-03-02)
[Full Changelog](https://github.com/chef/chef-server/compare/pc-rel-1.0.0.1...0.0.2)

## [pc-rel-1.0.0.1](https://github.com/chef/chef-server/tree/pc-rel-1.0.0.1) (2012-02-22)
[Full Changelog](https://github.com/chef/chef-server/compare/0.0.1...pc-rel-1.0.0.1)

## [0.0.1](https://github.com/chef/chef-server/tree/0.0.1) (2012-01-26)
[Full Changelog](https://github.com/chef/chef-server/compare/nofields-deploy...0.0.1)

## [nofields-deploy](https://github.com/chef/chef-server/tree/nofields-deploy) (2010-10-02)
[Full Changelog](https://github.com/chef/chef-server/compare/beta-1...nofields-deploy)

## [beta-1](https://github.com/chef/chef-server/tree/beta-1) (2010-09-09)


\* *This Change Log was automatically generated by [github_changelog_generator](https://github.com/skywinder/Github-Changelog-Generator)*