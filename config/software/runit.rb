name "runit"
version "2.1.1"

source :url => "http://smarden.org/runit/runit-2.1.1.tar.gz",
       :md5 => "8fa53ea8f71d88da9503f62793336bc3"

relative_path "admin"

working_dir = "#{project_dir}/runit-2.1.1"

build do
  # put runit where we want it, not where they tell us to
  command "sed -i -e s:^char *varservice =\"/service/\";$:char *varservice =\"/opt/opscode/service/\";: src/sv.c", :cwd => working_dir
  # TODO: the following is not idempotent
  command "sed -i -e s:/service:/opt/opscode/service: etc/2", :cwd => working_dir
  command "sed -i -e 's!^PATH=/command:/usr/local/bin:/usr/local/sbin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin$!PATH=/opt/opscode/bin:/opt/opscode/embedded/bin:/usr/local/bin:/usr/local/sbin:/bin:/sbin:/usr/bin:/usr/sbin!' etc/2", :cwd => working_dir
  command "sed -i -e s:-static:: src/Makefile", :cwd => working_dir

  # build it
  command "make", :cwd => "#{working_dir}/src"
  command "make check", :cwd => "#{working_dir}/src"

  # move it
  command "mkdir -p /opt/opscode/embedded/bin"
  ["src/chpst",
   "src/runit",
   "src/runit-init",
   "src/runsv",
   "src/runsvchdir",
   "src/runsvdir",
   "src/sv",
   "src/svlogd",
   "src/utmpset"].each do |bin|
    command "cp #{bin} /opt/opscode/embedded/bin", :cwd => working_dir
  end
  command "cp etc/2 /opt/opscode/embedded/bin/runsvdir-start", :cwd => working_dir

  # set up service directories
  %w{/opt/opscode/service /opt/opscode/sv /opt/opscode/init}.each do |dir|
    command "mkdir -p #{dir}"
  end
end
