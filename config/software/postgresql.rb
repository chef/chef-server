name "postgresql"
version "9.1.9"

dependencies ["zlib",
              "openssl",
              "libedit",
              "ncurses"]

source :url => "http://ftp.postgresql.org/pub/source/v9.1.9/postgresql-9.1.9.tar.gz",
       :md5 => "577f503a3fbabbe26145d0c6dae0b440"

relative_path "postgresql-9.1.9"

configure_env = {
  "LDFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "LD_RUN_PATH" => "#{install_dir}/embedded/lib"
}

build do
  patch :source => 'postgresql-9.1.9-configure-ncurses-fix.patch'
  command ["./configure",
           "--prefix=#{install_dir}/embedded",
           "--with-libedit-preferred",
           "--with-openssl --with-includes=#{install_dir}/embedded/include",
           "--with-libraries=#{install_dir}/embedded/lib"].join(" "), :env => configure_env
  command "make -j #{max_build_jobs}", :env => {"LD_RUN_PATH" => "#{install_dir}/embedded/lib"}
  command "make install"
end
