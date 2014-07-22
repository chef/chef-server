name "opscode-account"
default_version "rel-1.50.0"

dependency "ruby"
dependency "bundler"
dependency "postgresql92"
dependency "rsync"

source :git => "git@github.com:opscode/opscode-account"

relative_path "opscode-account"

# Since this project pulls in the pg gem (or depends on something that
# does) we need to have the pg_config binary on the PATH so the
# correct library and header locations can be found
env = {
  'PATH' => "#{install_dir}/embedded/bin:#{ENV['PATH']}"
}

bundle_path = "#{install_dir}/embedded/service/gem"

build do
  bundle "install --without test --path=#{bundle_path}", :env => env
  command "mkdir -p #{install_dir}/embedded/service/opscode-account"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/service/opscode-account/"

  # cleanup the .git directories in the bundle path before commiting
  # them as submodules to the git cache
  command "find #{bundle_path} -type d -name .git | xargs rm -rf"
end
