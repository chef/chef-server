name "opscode-test"
default_version "pc-rel-0.2.0.1"

dependencies ["ruby", "libxml2", "rsync", "bundler"]

source :git => "git@github.com:opscode/opscode-test"

# Since this project pulls in the pg gem (or depends on something that
# does) we need to have the pg_config binary on the PATH so the
# correct library and header locations can be found
env = {
  'PATH' => "#{install_dir}/embedded/bin:#{ENV['PATH']}"
}

bundle_path = "#{install_dir}/embedded/service/gem"

build do
  bundle "install --without mysql dev --path=#{bundle_path}", :env => env
  command "mkdir -p #{install_dir}/embedded/service/opscode-test"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/service/opscode-test/"

  # cleanup the .git directories in the bundle path before commiting
  # them as submodules to the git cache
  command "find #{bundle_path} -type d -name .git | xargs rm -rf"
end
