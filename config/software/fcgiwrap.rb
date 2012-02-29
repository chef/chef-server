name "fcgiwrap"
version "1.0.3"

dependencies ["autoconf", "fcgi"]

# TODO: deploy from the 1.0.3 tag / SHA
source :git => "git://github.com/gnosek/fcgiwrap"

relative_path "fcgiwrap"

env = {
  "LDFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "LD_RUN_PATH" => "#{install_dir}/embedded/lib",
  "PATH" => "#{install_dir}/embedded/bin:#{ENV["PATH"]}"
}

build do
  command "autoreconf -i", :env => env
  command "./configure --prefix=#{install_dir}/embedded", :env => env
  command "make -j #{max_build_jobs}", :env => {"LD_RUN_PATH" => "#{install_dir}/embedded/lib"}
  command "make install"
end
