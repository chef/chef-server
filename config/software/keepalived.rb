name "keepalived"
version "1.1.20"

dependencies ["popt"]

source :url => "http://www.keepalived.org/software/keepalived-1.1.20.tar.gz",
       :md5 => "6c3065c94bb9e2187c4b5a80f6d8be31"

relative_path "keepalived-1.1.20"

env = {
  "LDFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include -static-libgcc",
  "LD_RUN_PATH" => "#{install_dir}/embedded/lib"
}

build do
  command "./configure --prefix=#{install_dir}/embedded --with-include-popt --disable-iconv", :env => env
  command "make -j #{max_build_jobs}", :env => env
  command "make install"
end


