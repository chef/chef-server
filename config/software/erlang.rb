name "erlang"
version "R14B03"

dependencies ["zlib", "openssl"]

source :url => "http://www.erlang.org/download/otp_src_R14B03.tar.gz",
       :md5 => "7979e662d11476b97c462feb7c132fb7"

relative_path "otp_src_R14B03"

env = {
  "CFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include",
  "LDFLAGS" => "-Wl,-rpath /opt/opscode/embedded/lib -L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include"
}

build do
  # TODO: build cross-platform. this is for linux
  command(["./configure",
           "--prefix=/opt/opscode/embedded",
           "--enable-threads",
           "--enable-smp-support",
           "--enable-kernel-poll",
           "--enable-dynamic-ssl-lib",
           "--enable-shared-zlib",
           "--enable-hipe",
           "--without-javac",
           "--with-ssl=/opt/opscode/embedded",
           "--disable-debug"].join(" "),
          :env => env)

  command "make", :env => env
  command "make install"
end
