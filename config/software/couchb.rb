name "couchdb"
version "1.1.1"

dependencies ["spidermonkey", "icu", "curl", "erlang"]

source :url => "http://archive.apache.org/dist/couchdb/1.1.1/apache-couchdb-1.1.1.tar.gz",
       :md5 => "cd126219b9cb69a4c521abd6960807a6"

relative_path "apache-couchdb-1.1.1"

build_env = {
  "RPATH" => "#{install_dir}/embedded/lib",
  "CURL_CONFIG" => "#{install_dir}/embedded/bin/curl-config",
  "ICU_CONFIG" => "#{install_dir}/embedded/bin/icu-config",
  "ERL" => "#{install_dir}/embedded/bin/erl",
  "ERLC" => "#{install_dir}/embedded/bin/erlc",
  "LD_RUN_PATH" => "#{install_dir}/embedded/lib",
  "CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "PATH" => "#{install_dir}/embedded/bin:#{ENV["PATH"]}"
}

build do
#  command "./bootstrap", :env => build_env
  command ["./configure",
           "--prefix=#{install_dir}/embedded",
           "--disable-init",
           "--disable-launchd",
           "--with-erlang=#{install_dir}/embedded/lib/erlang/usr/include",
           "--with-js-include=#{install_dir}/embedded/include",
           "--with-js-lib=#{install_dir}/embedded/lib"].join(" "), :env => build_env
  command "make", :env => build_env
  command "make install", :env => build_env
end
