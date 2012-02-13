name "opscode-org-creator"

dependencies ["erlang", "rsync"]

source :git => "git@github.com:opscode/opscode-org-creator"

relative_path "opscode-org-creator"

env = {
  "PATH" => "/opt/opscode/embedded/bin:#{ENV["PATH"]}",
  "LD_FLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include",
  "LD_RUN_PATH" => "/opt/opscode/embedded/lib"
}

build do
  # TODO: we need to get the app.config file there before we build ...
  # or do we?
  command "make rel", :env => env
  command "mkdir -p /opt/opscode/embedded/service/opscode-org-creator"
  command "/opt/opscode/embedded/bin/rsync -a ./ /opt/opscode/embedded/service/opscode-org-creator/"
end
