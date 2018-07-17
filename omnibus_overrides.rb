override :erlang, version: "18.3"
override :lua, version: "5.1.5"
override :'omnibus-ctl', version: "master"
override :chef, version: "v14.3.0"
override :ohai, version: "v14.3.0"
override :ruby, version: "2.5.1"
override :rubygems, version: "2.7.7"
# This SHA is the last commit before the 6.0 release
override :'berkshelf-no-depselector', version: '6016ca10b2f46508b1b107264228668776f505d9'

# Note 2018/02/01 (sr): This is related to issue #1417:
# This is the last version that supports --with-lua51=PATH, which allows us to
# build it with lua on ppc64[le] and s390x. Those platforms are not supported
# in mainline luajit. There's forks for ppc64, and s390x, but going forward with
# those was so far blocked by ppc64 not being supported even with the PPC64
# fork.
override :openresty, version: "1.11.2.1"
