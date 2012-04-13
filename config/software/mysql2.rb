name "mysql2"
versions_to_install = [ "0.3.6", "0.3.7" ]
version versions_to_install.join("-")

dependencies ["ruby",
              "bundler"]


build do
  gem "install rake-compiler"
  command "mkdir -p #{install_dir}/embedded/service/gem/ruby/1.9.1/cache"
  versions_to_install.each do |ver|
    gem "fetch mysql2 --version #{ver}", :cwd => "#{install_dir}/embedded/service/gem/ruby/1.9.1/cache"
  end
end

