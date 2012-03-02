name "spawn-fcgi"
version "1.6.3"

dependencies ["fcgi", "fcgiwrap"]

source :url => "http://www.lighttpd.net/download/spawn-fcgi-1.6.3.tar.gz",
       :md5 => "6d75f9e9435056fa1e574d836d823cd0"

relative_path "spawn-fcgi-1.6.3"

configure_env = {
  "LDFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "LD_RUN_PATH" => "#{install_dir}/embedded/lib",
  "PATH" => "#{install_dir}/embedded/bin:#{ENV["PATH"]}"
}

build do
  command "./configure --prefix=#{install_dir}/embedded", :env => configure_env
  command "make -j #{max_build_jobs}", :env => {"LD_RUN_PATH" => "#{install_dir}/embedded/lib"}
  command "make install"
end
