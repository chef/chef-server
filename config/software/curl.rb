name "curl"
version "7.23.1"

dependencies ["zlib", "openssl"]

source :url => "http://curl.haxx.se/download/curl-7.23.1.tar.gz",
       :md5 => "8e23151f569fb54afef093ac0695077d"

relative_path 'curl-7.23.1'

build do
  command ["./configure",
           "--prefix=#{install_dir}/embedded",
           "--disable-debug",
           "--enable-optimize",
           "--disable-ldap",
           "--disable-ldaps",
           "--disable-rtsp",
           "--enable-proxy",
           "--disable-dependency-tracking",
           "--enable-ipv6",
           "--without-libidn",
           "--with-ssl=#{install_dir}/embedded",
           "--with-zlib=#{install_dir}/embedded"].join(" ")

  command "make -j #{max_build_jobs}", :env => {"LD_RUN_PATH" => "#{install_dir}/embedded/lib"}
  command "make install"
end
