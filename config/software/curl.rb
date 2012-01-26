name "curl"

source :url => "http://curl.haxx.se/download/curl-7.23.1.tar.gz",
       :md5 => "8e23151f569fb54afef093ac0695077d"

relative_path 'curl-7.23.1'

build do
  command ["./configure",
           "--prefix=/opt/opscode/embedded",
           "--disable-debug",
           "--enable-optimize",
           "--disable-ldap",
           "--disable-ldaps",
           "--disable-rtsp",
           "--enable-proxy",
           "--disable-dependency-tracking",
           "--enable-ipv6",
           "--without-libidn",
           "--with-ssl=/opt/opscode/embedded",
           "--with-zlib=/opt/opscode/embedded"].join(" ")

  command "make", :env => {"LD_RUN_PATH" => "/opt/opscode/embedded/lib"}
  command "make install"
end
