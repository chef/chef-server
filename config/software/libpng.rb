name "libpng"
version "1.5.9"

dependencies ["zlib"]

source :url => "ftp://ftp.simplesystems.org/pub/libpng/png/src/libpng-#{version}.tar.gz",
       :md5 => "c740ba66cd7074ba2471b6a4ff48e1fb"

relative_path "libpng-#{version}"

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
