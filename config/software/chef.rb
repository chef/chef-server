name "chef"

dependencies ["ruby", "rubygems", "libxml2", "libxslt"]

build do
  command ["/opt/opscode/embedded/bin/gem install chef",
           "-n /opt/opscode/bin",
           "--no-rdoc --no-ri --",
           "--with-xml2-include=/opt/opscode/embedded/include/libxml2",
           "--with-xml2-lib=/opt/opscode/embedded/lib"].join(" ")

  command ["/opt/opscode/embedded/bin/gem install",
           "highline net-ssh-multi knife-ec2 knife-rackspace",
           "-n /opt/opscode/bin",
           "--no-rdoc --no-ri --",
           "--with-xml2-include=/opt/opscode/embedded/include/libxml2",
           "--with-xml2-lib=/opt/opscode/embedded/lib"].join(" ")

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
