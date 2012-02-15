name "spawn-fcgi"

source :url => "http://www.lighttpd.net/download/spawn-fcgi-1.6.3.tar.gz",
       :md5 => "6d75f9e9435056fa1e574d836d823cd0"

relative_path "spawn-fcgi-1.6.3"

configure_env = {
  "LDFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include",
  "CFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include",
  "LD_RUN_PATH" => "/opt/opscode/embedded/lib",
  "PATH" => "/opt/opscode/embedded/bin:#{ENV["PATH"]}"
}

build do
  command "./configure --prefix=/opt/opscode/embedded", :env => configure_env
  command "make", :env => {"LD_RUN_PATH" => "/opt/opscode/embedded/lib"}
  command "make install"
end
