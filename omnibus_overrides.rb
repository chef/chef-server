#
# When updating this, check doc/FrequentTasks.md for checklists to ensure all
# the various usages are updated in lockstep
#
override :erlang, version: "22.2"
override :'omnibus-ctl', version: "master"
override :chef, version: "v16.13.16"
override :ohai, version: "v16.13.0"
override :ruby, version: "2.6.7"
override :perl, version: "5.18.1"
override :bundler, version: "2.1.4" # due to how we install chef we need the bundler to match the gemfile.lock there

override :cpanminus, version: "1.7004" # 1.9019 breaks installs currently
override :logrotate, version: "3.9.2" # 3.18.0 patches fail

override :openresty, version: "1.19.3.1"
