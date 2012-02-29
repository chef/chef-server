name "fcgi"
version "2.4.0"

dependencies ["autoconf", "automake", "libtool"]

source :url => "http://fastcgi.com/dist/fcgi-2.4.0.tar.gz",
       :md5 => "d15060a813b91383a9f3c66faf84867e"

relative_path "fcgi-2.4.0"

reconf_env = {"PATH" => "#{install_dir}/embedded/bin:#{ENV["PATH"]}"}

configure_env = {
  "LDFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include -L/lib -L/usr/lib",
  "CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "LD_RUN_PATH" => "#{install_dir}/embedded/lib",
  "PATH" => "#{install_dir}/embedded/bin:#{ENV["PATH"]}"
}

build do
  # patch and touch files so it builds
  diff = <<D
24a25
> #include <cstdio>
D
  command "echo '#{diff}' | patch libfcgi/fcgio.cpp"
  command "touch COPYING ChangeLog AUTHORS NEWS"

  # autoreconf
  command "autoreconf -i -f", :env => reconf_env
  command "libtoolize", :env => reconf_env

  # configure and build
  command "./configure --prefix=#{install_dir}/embedded", :env => configure_env
  command "make -j #{max_build_jobs}", :env => {"LD_RUN_PATH" => "#{install_dir}/embedded/lib"}
  command "make install"
end
