name "php"
version "5.3.10"

dependencies ["zlib",
              "pcre",
              "libxslt",
              "libxml2",
              "libiconv",
              "openssl",
              "gd"]

source :url => "http://us.php.net/distributions/php-5.3.10.tar.gz",
       :md5 => "2b3d2d0ff22175685978fb6a5cbcdc13"

relative_path "php-5.3.10"

env = {
  "LDFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include",
  "CFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include",
  "LD_RUN_PATH" => "/opt/opscode/embedded/lib"
}

build do
  command(["./configure",
           "--prefix=/opt/opscode/embedded",
           "--without-pear",
           "--with-zlib-dir=/opt/opscode/embedded",
           "--with-pcre-dir=/opt/opscode/embedded",
           "--with-xsl=/opt/opscode/embedded",
           "--with-libxml-dir=/opt/opscode/embedded",
           "--with-iconv=/opt/opscode/embedded",
           "--with-openssl-dir=/opt/opscode/embedded",
           "--with-gd=/opt/opscode/embedded",
           "--enable-fpm",
           "--with-fpm-user=opscode",
           "--with-fpm-group=opscode"].join(" "),
          :env => env)

  command "make", :env => {"LD_RUN_PATH" => "/opt/opscode/embedded/lib"}
  command "make install"
end
