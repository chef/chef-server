name "chef-gem"
version "0.10.10"

dependencies ["ruby", "rubygems", "yajl"]

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
