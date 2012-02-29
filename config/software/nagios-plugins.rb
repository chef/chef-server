name "nagios-plugins"
version "1.4.15"

dependencies ["zlib", "openssl", "postgresql", "libiconv"]

# the url is the location of a redirect from sourceforge
source :url => "http://voxel.dl.sourceforge.net/project/nagiosplug/nagiosplug/1.4.15/nagios-plugins-1.4.15.tar.gz",
       :md5 => "56abd6ade8aa860b38c4ca4a6ac5ab0d"

relative_path "nagios-plugins-1.4.15"

configure_env = {
  "LDFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "LD_RUN_PATH" => "#{install_dir}/embedded/lib"
}

gem_env = {"GEM_PATH" => nil, "GEM_HOME" => nil}

build do
  # configure it
  command(["./configure",
           "--prefix=#{install_dir}/embedded/nagios",
           "--with-trusted-path=#{install_dir}/bin:#{install_dir}/embedded/bin:/bin:/sbin:/usr/bin:/usr/sbin",
           "--with-openssl=#{install_dir}/embedded",
           "--with-pgsql=#{install_dir}/embedded",
           "--with-libiconv-prefix=#{install_dir}/embedded"].join(" "),
          :env => configure_env)

  # build it
  command "make -j #{max_build_jobs}", :env => {"LD_RUN_PATH" => "#{install_dir}/embedded/lib"}
  command "sudo make install"
end
