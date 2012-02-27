name "spidermonkey"
version "1.8.0"

source :url => "http://ftp.mozilla.org/pub/mozilla.org/js/js-1.8.0-rc1.tar.gz",
       :md5 => "eaad8815dcc66a717ddb87e9724d964e"

relative_path "js"

env = {"LD_RUN_PATH" => "#{install_dir}/embedded/lib"}
working_dir = "#{project_dir}/src"

# == Build Notes ==
# The spidermonkey build instructions are copied from here:
# http://wiki.apache.org/couchdb/Installing_SpiderMonkey
#
# These instructions only seem to work with spidermonkey 1.8.0-rc1 and
# earlier. Since couchdb 1.1.1 is compatible with spidermonkey 1.8.5,
# we should eventually invest some time into getting that version built.
#

build do
  command(["make",
           "BUILD_OPT=1",
           "XCFLAGS=-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
           "-f",
           "Makefile.ref"].join(" "),
          :env => env,
          :cwd => working_dir)
  command(["make",
           "BUILD_OPT=1",
           "JS_DIST=#{install_dir}/embedded",
           "-f",
           "Makefile.ref",
           "export"].join(" "),
          :env => env,
          :cwd => working_dir)

  # TODO: only do the following on 64-bit linux
  command "mv #{install_dir}/embedded/lib64/libjs.a #{install_dir}/embedded/lib"
  command "mv #{install_dir}/embedded/lib64/libjs.so #{install_dir}/embedded/lib"
  command "rm -rf #{install_dir}/embedded/lib64"
end
