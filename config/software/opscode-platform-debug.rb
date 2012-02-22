name "opscode-platform-debug"

dependencies ["ruby", "bundler", "postgresql", "rsync"]

source :git => "git@github.com:opscode/opscode-platform-debug"

relative_path "opscode-plaform-debug"

bundle_env = {"GEM_HOME" => nil, "GEM_PATH" => nil}
orgmapper_dir = "#{project_dir}/orgmapper"

build do
  # bundle install orgmapper
  command "/opt/opscode/embedded/bin/bundle install --without mysql", :env => bundle_env, :cwd => orgmapper_dir

  command "mkdir -p /opt/opscode/embedded/service/opscode-platform-debug"
  command "/opt/opscode/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ /opt/opscode/embedded/service/opscode-platform-debug/"
end
