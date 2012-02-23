name "chef-pc"
version "0.10.8"

dependencies ["ruby", "rubygems"]

build do
  command ["/opt/opscode/embedded/bin/gem install chef",
           "-v #{version}",
           "-n /opt/opscode/bin",
           "--no-rdoc --no-ri"].join(" ")

  # don't need to do the symlinking setup for private chef

  # clean up
  ["docs",
   "share/man",
   "share/doc",
   "share/gtk-doc",
   "ssl/man",
   "man",
   "info"].each do |dir|
    command "rm -rf /opt/opscode/embedded/#{dir}"
  end
end
