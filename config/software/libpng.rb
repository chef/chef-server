name "libpng"
version "1.5.8"

dependencies ["zlib"]

source :url => "ftp://ftp.simplesystems.org/pub/libpng/png/src/libpng-1.5.8.tar.gz",
       :md5 => "dc2b84a1c077531ceb5bf9d79ad889a4"

relative_path "libpng-1.5.8"

configure_env = {
  "LDFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include",
  "CFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include",
  "LD_RUN_PATH" => "/opt/opscode/embedded/lib"
}

build do
  command "./configure --prefix=/opt/opscode/embedded --with-zlib-prefix=/opt/opscode/embedded", :env => configure_env
  command "make", :env => {"LD_RUN_PATH" => "/opt/opscode/embedded/lib"}
  command "make install"
end
