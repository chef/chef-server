name "mysql2"
version "0.3.6"

dependencies ["ruby",
              "bundler"]

build do
  gem "install rake-compiler"
  command "mkdir -p #{install_dir}/embedded/service/gem/ruby/1.9.1/cache"
  gem "fetch mysql2 --version #{version}", :cwd => "#{install_dir}/embedded/service/gem/ruby/1.9.1/cache"
end

