name "nginx"
version "1.0.12"

dependencies ["pcre"]

source :url => "http://nginx.org/download/nginx-1.0.12.tar.gz",
       :md5 => "d0ceefeb2a68ecb19e78ee894a5b52a3"

relative_path "nginx-1.0.12"

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
