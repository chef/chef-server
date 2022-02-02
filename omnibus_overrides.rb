#
# When updating this, check doc/FrequentTasks.md for checklists to ensure all
# the various usages are updated in lockstep
#
override :erlang, version: "22.2"
override :'omnibus-ctl', version: "main"
override :chef, version: "v16.17.4"
override :ohai, version: "v16.17.0"
override :ruby, version: "2.7.5"
override :perl, version: "5.18.1"
override :redis, version: "5.0.14"
override :runit, version: "2.1.1" #standalone upgrade is failing, Needs to be reverted to 2.1.2 after fixing the umbrella

override :cpanminus, version: "1.7004" # 1.9019 breaks installs currently
override :logrotate, version: "3.9.2" # 3.18.0 patches fail

# update `src/openresty-noroot/habitat/plan.sh` when updating this version.
override :openresty, version: "1.19.9.1"
