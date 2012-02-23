name "fcgiwrap"
version "1.0.3"

dependencies ["autoconf", "fcgi"]

# TODO: deploy from the 1.0.3 tag / SHA
source :git => "git://github.com/gnosek/fcgiwrap"

relative_path "fcgiwrap"

env = {
  "LDFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include",
  "CFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include",
  "LD_RUN_PATH" => "/opt/opscode/embedded/lib",
  "PATH" => "/opt/opscode/embedded/bin:#{ENV["PATH"]}"
}

build do
  command "autoreconf -i", :env => env
  command "./configure --prefix=/opt/opscode/embedded", :env => env
  command "make", :env => {"LD_RUN_PATH" => "/opt/opscode/embedded/lib"}
  command "make install"
end
