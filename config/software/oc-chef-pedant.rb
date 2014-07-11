name "oc-chef-pedant"
default_version "dt/system-recovery2"

dependency "ruby"
dependency "bundler"
dependency "rsync"

source :git => "git@github.com:opscode/oc-chef-pedant.git"

relative_path "oc-chef-pedant"

bundle_path = "#{install_dir}/embedded/service/gem"

build do
  bundle "install --path=#{bundle_path}"
  command "mkdir -p #{install_dir}/embedded/service/oc-chef-pedant"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/service/oc-chef-pedant/"

  # cleanup the .git directories in the bundle path before commiting
  # them as submodules to the git cache
  command "find #{bundle_path} -type d -name .git | xargs rm -rf"
end
