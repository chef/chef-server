name "nginx"
version "1.2.3"

dependencies ["pcre"]

source :url => "http://nginx.org/download/nginx-1.2.3.tar.gz",
       :md5 => "0a986e60826d9e3b453dbefc36bf8f6c"

relative_path "nginx-1.2.3"

build do
  command ["./configure",
           "--prefix=#{install_dir}/embedded",
           "--with-http_ssl_module",
           "--with-debug",
           "--with-ld-opt=-L#{install_dir}/embedded/lib",
           "--with-cc-opt=\"-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include\""].join(" ")
  command "make -j #{max_build_jobs}", :env => {"LD_RUN_PATH" => "#{install_dir}/embedded/lib"}
  command "make install"
end
