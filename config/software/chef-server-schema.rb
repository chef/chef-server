name "chef-server-schema"
version "1.0.3"

dependency "sqitch"

# TODO: Use https when this is finally open-sourced
source :git => "git@github.com:opscode/chef-server-schema.git"

build do
  command "mkdir -p #{install_dir}/embedded/service/#{name}"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/service/#{name}/"
end
