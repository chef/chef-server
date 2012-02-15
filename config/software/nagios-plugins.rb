name "nagios-plugins"

dependencies ["zlib", "openssl", "postgresql", "libiconv"]

# the url is the location of a redirect from sourceforge
source :url => "http://voxel.dl.sourceforge.net/project/nagiosplug/nagiosplug/1.4.15/nagios-plugins-1.4.15.tar.gz",
       :md5 => "56abd6ade8aa860b38c4ca4a6ac5ab0d"

relative_path "nagios-plugins-1.4.15"

configure_env = {
  "LDFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include",
  "CFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include",
  "LD_RUN_PATH" => "/opt/opscode/embedded/lib"
}

gem_env = {"GEM_PATH" => nil, "GEM_HOME" => nil}

build do
  # configure it
  command(["./configure",
           "--prefix=/opt/opscode/embedded/nagios",
           "--with-trusted-path=/opt/opscode/bin:/opt/opscode/embedded/bin:/bin:/sbin:/usr/bin:/usr/sbin",
           "--with-openssl=/opt/opscode/embedded",
           "--with-pgsql=/opt/opscode/embedded",
           "--with-libiconv-prefix=/opt/opscode/embedded"].join(" "),
          :env => configure_env)

  # build it
  command "make", :env => {"LD_RUN_PATH" => "/opt/opscode/embedded/lib"}
  command "make install"
end
