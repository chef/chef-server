name "opscode-org-creator"
default_version "rel-1.1.0"

dependencies ["erlang", "rebar", "rsync"]

source :git => "git@github.com:opscode/opscode-org-creator"

relative_path "opscode-org-creator"

env = {
  "PATH" => "#{install_dir}/embedded/bin:#{ENV["PATH"]}",
  "LDFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "LD_RUN_PATH" => "#{install_dir}/embedded/lib"
}

build do
  # TODO: we need to get the app.config file there before we build ...
  # or do we?
  command "make rel", :env => env
  command "mkdir -p #{install_dir}/embedded/service/opscode-org-creator"
  command "#{install_dir}/embedded/bin/rsync -a ./ #{install_dir}/embedded/service/opscode-org-creator/"
  command "rm -rf #{install_dir}/embedded/service/opscode-org-creator/rel/org_app/log"
end
