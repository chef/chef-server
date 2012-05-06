name "chef-git"

version ENV['chef_git']

dependencies ["ruby", "rubygems", "yajl"]

source :git => "git@github.com:opscode/chef"

relative_path "chef"

build do

  command "rake gem"

  gem ["install chef/pkg/chef*.gem",
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
