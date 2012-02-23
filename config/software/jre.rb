name "jre"
version "7u2-b13"

dependencies ["rsync"]

# TODO: download x86 version on x86 machines
source :url => "http://download.oracle.com/otn-pub/java/jdk/7u2-b13/jdk-7u2-linux-x64.tar.gz",
       :md5 => "a0bbb9265b4633cfd7823928649f450c"

relative_path "jdk1.7.0_02"

jre_dir = "/opt/opscode/embedded/jre"

build do
  command "mkdir -p #{jre_dir}"
  command "/opt/opscode/embedded/bin/rsync -a jre/ #{jre_dir}/"
end
