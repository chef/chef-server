build_version   "12.15.8"
build_iteration 1

override :erlang, version: "18.3"
override :lua, version: "5.1.5"
override :'omnibus-ctl', version: "master"
override :chef, version: "v13.1.31"
override :ohai, version: "v13.1.0"
override :ruby, version: "2.4.1"
override :rubygems, version: "2.6.8"
# This SHA is the last commit before the 6.0 release
override :'berkshelf-no-depselector', version: '6016ca10b2f46508b1b107264228668776f505d9'
