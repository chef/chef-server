name "nrpe"
version "2.13"

dependencies ["zlib", "openssl"]

# tarball location comes from sourceforge download redirect
source :url => "http://voxel.dl.sourceforge.net/project/nagios/nrpe-2.x/nrpe-2.13/nrpe-2.13.tar.gz",
       :md5 => "e5176d9b258123ce9cf5872e33a77c1a"

relative_path "nrpe-2.13"

env = {
  "LDFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include",
  "CFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include",
  "LD_RUN_PATH" => "/opt/opscode/embedded/lib",
  "PATH" => "/opt/opscode/embedded/bin:#{ENV["PATH"]}"
}

build do
  # TODO: OMG THIS IS HORRIBLE
  command "sed -i 's:\\r::g' ./src/nrpe.c"

  # TODO: add real semantics around patching
  patch_file = File.expand_path("../config/patches/nrpe/fix_for_runit.patch", __FILE__)
  command "cat #{patch_file} | patch -p1 ./src/nrpe.c"

  # configure it
  command(["./configure",
           "--prefix=/opt/opscode/embedded",
           "--with-ssl=/opt/opscode/embedded",
           "--with-ssl-lib=/opt/opscode/embedded/lib",
           "--with-ssl-inc=/opt/opscode/embedded/include"].join(" "),
          :env => env)

  # build it
  command "make all", :env => {"LD_RUN_PATH" => "/opt/opscode/embedded/lib"}

  # move it
  command "mkdir -p /opt/opscode/embedded/nagios/libexec"
  command "cp ./src/check_nrpe /opt/opscode/embedded/nagios/libexec"
  command "cp ./src/nrpe /opt/opscode/embedded/bin"
end
