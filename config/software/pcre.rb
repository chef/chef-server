name "pcre"
version "8.30"

source :url => "ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/pcre-8.30.tar.gz",
       :md5 => "d5ee0d9f6d2f0b7489331d04b6c182ef"

relative_path "pcre-8.30"

build do
  command("./configure --prefix=/opt/opscode/embedded",
          :env => {
            "CFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include"
          })
  # command "touch alocal.m4"
  command("make",
          :env => {
            "PATH" => "/opt/opscode/embedded/bin:#{ENV["PATH"]}"
          })
  command "make install"
end
