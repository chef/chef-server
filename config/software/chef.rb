name "chef-pc"
version "0.10.8"

dependencies ["ruby", "rubygems"]

build do
  command ["#{install_dir}/embedded/bin/gem install chef",
           "-v #{version}",
           "-n #{install_dir}/bin",
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
    command "rm -rf #{install_dir}/embedded/#{dir}"
  end
end
