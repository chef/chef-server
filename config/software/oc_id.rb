name "oc_id"
default_version "0.4.4"

dependency "postgresql92" # for libpq
dependency "nodejs"
dependency "ruby"
dependency "bundler"

source :git => "git@github.com:opscode/oc-id"

relative_path "oc-id"

env = {
  "PATH" => "#{install_dir}/embedded/bin:#{ENV["PATH"]}",
  "LD_FLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "LD_RUN_PATH" => "#{install_dir}/embedded/lib"
}

build do
  bundle "install --path=#{install_dir}/embedded/service/gem --without development test doc", :env => env
  bundle "exec rake assets:precompile", :env => env
  command "mkdir -p #{install_dir}/embedded/service/oc_id"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore --exclude=log --exclude=tmp ./ #{install_dir}/embedded/service/oc_id/"
  command "rm -rf #{install_dir}/embedded/service/oc_id/log"
end
