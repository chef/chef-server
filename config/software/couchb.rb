name "couchdb"

dependencies ["spidermonkey", "icu", "curl", "erlang"]

source :url => "http://archive.apache.org/dist/couchdb/1.1.1/apache-couchdb-1.1.1.tar.gz",
       :md5 => "cd126219b9cb69a4c521abd6960807a6"

relative_path "apache-couchdb-1.1.1"

build_env = {
  "RPATH" => "/opt/opscode/embedded/lib",
  "CURL_CONFIG" => "/opt/opscode/embedded/bin/curl-config",
  "ICU_CONFIG" => "/opt/opscode/embedded/bin/icu-config",
  "ERL" => "/opt/opscode/embedded/bin/erl",
  "ERLC" => "/opt/opscode/embedded/bin/erlc",
  "LD_RUN_PATH" => "/opt/opscode/embedded/lib",
  "CFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include",
  "PATH" => "/opt/opscode/embedded/bin:#{ENV["PATH"]}"
}

build do
#  command "./bootstrap", :env => build_env
  command ["./configure",
           "--prefix=/opt/opscode/embedded",
           "--disable-init",
           "--disable-launchd",
           "--with-erlang=/opt/opscode/embedded/lib/erlang/usr/include",
           "--with-js-include=/opt/opscode/embedded/include",
           "--with-js-lib=/opt/opscode/embedded/lib"].join(" "), :env => build_env
  command "make", :env => build_env
  command "make install", :env => build_env
end
