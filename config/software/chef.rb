name "chef"
version "0.10.8"

dependencies ["ruby", "rubygems"]

build do
  gem ["install chef",
      "-v #{version}",
      "-n #{install_dir}/bin",
      "--no-rdoc --no-ri"].join(" ")

  gem ["install highline net-ssh-multi", # TODO: include knife gems?
       "-n #{install_dir}/bin",
       "--no-rdoc --no-ri"].join(" ")

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
