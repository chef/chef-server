# NOTE:
# Although the contents of this definition are identical to the 'chef'
# definition, we have created them seperately to prepare for the event
# that the versions of OSS chef-client and OPC-embedded chef-client
# are tracked differently.

name "chef-pc"
version "0.10.8"

dependencies ["ruby", "rubygems"]

build do
  gem ["install chef",
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
