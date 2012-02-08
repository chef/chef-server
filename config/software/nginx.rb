name "nginx"

dependencies ["pcre"]

source :url => "http://nginx.org/download/nginx-1.0.12.tar.gz",
       :md5 => "d0ceefeb2a68ecb19e78ee894a5b52a3"

relative_path "nginx-1.0.12"

build do
  command ["./configure",
           "--prefix=/opt/opscode/embedded",
           "--with-http_ssl_module",
           "--with-debug",
           "--with-ld-opt=-L/opt/opscode/embedded/lib",
           "--with-cc-opt=\"-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include\""].join(" ")
  command "make", :env => {"LD_RUN_PATH" => "/opt/opscode/embedded/lib"}
  command "make install"
end
