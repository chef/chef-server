name "keepalived"
version "1.2.2"

dependencies ["popt"]

source :url => "http://www.keepalived.org/software/keepalived-1.2.2.tar.gz",
       :md5 => "f68327ca142616a8463d2d105db122cd"

relative_path "keepalived-1.2.2"

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


