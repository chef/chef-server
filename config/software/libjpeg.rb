name "libjpeg"

source :url => "http://www.ijg.org/files/jpegsrc.v8d.tar.gz",
       :md5 => "52654eb3b2e60c35731ea8fc87f1bd29"

relative_path "jpeg-8d"

configure_env = {
  "LDFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include",
  "CFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include",
  "LD_RUN_PATH" => "/opt/opscode/embedded/lib",
  "PATH" => "/opt/opscode/embedded/bin:#{ENV["PATH"]}"
}

build do
  command "./configure --prefix=/opt/opscode/embedded --enable-shared --enable-static", :env => configure_env
  command "mkdir -p /opt/opscode/embedded/man/man1"
  command "make", :env => {"LD_RUN_PATH" => "/opt/opscode/embedded/lib"}
  command "make install"
  command "rm -rf /opt/opscode/embedded/man"
end
