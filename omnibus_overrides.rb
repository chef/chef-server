#
# When updating this, check doc/FrequentTasks.md for checklists to ensure all
# the various usages are updated in lockstep
#
override :erlang, version: "22.2"
override :'omnibus-ctl', version: "main"
override :chef, version: "v16.17.51"
override :ohai, version: "v16.17.0"
override :ruby, version: "2.7.5"
override :perl, version: "5.34.0"
override :redis, version: "6.2.7"
override :runit, version: "2.1.1" #standalone upgrade is failing, Needs to be reverted to 2.1.2 after fixing the umbrella
override :sqitch, version: "0.973"

override :logrotate, version: "3.9.2" # 3.18.0 patches fail

# update `src/openresty-noroot/habitat/plan.sh` when updating this version.
override :openresty, version: "1.21.4.1rc1"
