#
# When updating this, check doc/FrequentTasks.md for checklists to ensure all
# the various usages are updated in lockstep
#
override :erlang, version: "26.2.5.14"
override :'omnibus-ctl', version: "main"
override :chef, version: "v18.6.1"
override :ohai, version: "v16.17.0"
override :ruby, version: "3.1.7"
override :perl, version: "5.34.0"
override :redis, version: "5.0.14"
override :runit, version: "2.1.1" #standalone upgrade is failing, Needs to be reverted to 2.1.2 after fixing the umbrella
override :sqitch, version: "0.973"

override :logrotate, version: "3.19.0"

# update `src/openresty-noroot/habitat/plan.sh` when updating this version.
override :openresty, version: "1.25.3.1"

override :openssl, version: "3.2.4"