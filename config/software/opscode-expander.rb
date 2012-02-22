name "opscode-expander"

dependencies ["ruby", "bundler", "rsync"]

source :git => "git@github.com:opscode/opscode-expander"

relative_path "opscode-expander"

bundle_env = {"GEM_PATH" => nil, "GEM_HOME" => nil}

build do
  command "/opt/opscode/embedded/bin/bundle install --without mysql"
  command "mkdir -p /opt/opscode/embedded/service/opscode-expander"
  command "/opt/opscode/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ /opt/opscode/embedded/service/opscode-expander/"
end
