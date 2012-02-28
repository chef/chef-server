name "couchdb"
version "1.0.3"

dependencies ["spidermonkey", "icu", "curl", "erlang"]

source :url => "http://archive.apache.org/dist/couchdb/#{version}/apache-couchdb-#{version}.tar.gz",
       :md5 => "cfdc2ab751bf18049c5ef7866602d8ed"

relative_path "apache-couchdb-#{version}"

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
