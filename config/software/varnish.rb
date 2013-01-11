name "varnish"
version "3.0.3"

dependencies ["pcre", "ncurses"]

source :url => "http://repo.varnish-cache.org/source/varnish-3.0.3.tar.gz",
       :md5 => "714310c83fdbd2061d897dacd3f63d8b"

relative_path "varnish-3.0.3"

env =
  {
  "LDFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include -I#{install_dir}/embedded/include/ncurses -static-libgcc",
  "LD_RUN_PATH" => "#{install_dir}/embedded/lib",
  "PCRE_CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include -static-libgcc",
  "PCRE_LIBS" => "-L#{install_dir}/embedded/lib -lpcre"
}

build do
  command "./configure --prefix=#{install_dir}/embedded", :env => env
  command "make", :env => env
  command "make install"
end
