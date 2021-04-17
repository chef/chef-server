#
# When updating this, check doc/FrequentTasks.md for checklists to ensure all
# the various usages are updated in lockstep
#
override :erlang, version: "22.2"
override :'omnibus-ctl', version: "master"
override :chef, version: "v15.17.4"
override :ohai, version: "v15.12.0"
override :ruby, version: "2.6.7"
override :perl, version: "5.18.1"

# This SHA is the last commit before the 6.0 release
override :'berkshelf-no-depselector', version: '6016ca10b2f46508b1b107264228668776f505d9'

override :openresty, version: "1.19.3.1"
