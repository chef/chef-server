name "erlang"
version "R14B03"

dependencies ["zlib", "openssl", "ncurses"]

source :url => "http://www.erlang.org/download/otp_src_R14B03.tar.gz",
       :md5 => "7979e662d11476b97c462feb7c132fb7"

relative_path "otp_src_R14B03"

env = {
  "CFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/erlang/include",
  "LDFLAGS" => "-Wl,-rpath /opt/opscode/embedded/lib -L/opt/opscode/embedded/lib -I/opt/opscode/embedded/erlang/include"
}

build do
  # set up the erlang include dir
  command "mkdir -p /opt/opscode/embedded/erlang/include"
  %w{ncurses openssl zlib.h zconf.h}.each do |link|
    command "ln -fs /opt/opscode/embedded/include/#{link} /opt/opscode/embedded/erlang/include/#{link}"
  end

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
