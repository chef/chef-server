name "postgresql"

dependencies ["zlib",
              "openssl"]

source :url => "http://ftp.postgresql.org/pub/source/v9.1.2/postgresql-9.1.2.tar.gz",
       :md5 => "fe01293f96e04da9879840b1996a3d2c"

relative_path "postgresql-9.1.2"

configure_env = {
  "LDFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include",
  "CFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include",
  "LD_RUN_PATH" => "/opt/opscode/embedded/lib"
}

build do
  command ["./configure",
           "--prefix=/opt/opscode/embedded",
           "--with-openssl --with-includes=/opt/opscode/embedded/include",
           "--with-libraries=/opt/opscode/embedded/lib"].join(" "), :env => configure_env
  command "make", :env => {"LD_RUN_PATH" => "/opt/opscode/embedded/lib"}
  command "make install"
end
