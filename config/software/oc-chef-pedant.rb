name "oc-chef-pedant"
default_version "1.0.50"

dependency "ruby"
dependency "bundler"
dependency "rsync"

source :git => "git@github.com:opscode/oc-chef-pedant.git"

relative_path "oc-chef-pedant"

required_files = [
                  'HEAD',
                  'description',
                  'hooks',
                  'info',
                  'objects',
                  'refs',
                  ]

bundle_path = "#{install_dir}/embedded/service/gem"

build do
  bundle "install --path=#{bundle_path}"
  command "mkdir -p #{install_dir}/embedded/service/oc-chef-pedant"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/service/oc-chef-pedant/"

  # cleanup the .git directories in the bundle path before commiting
  # them as submodules to the git cache
  Dir.glob("#{install_dir}/**/config").reject{ |path|
    required_files.any? { |required_file|
      !File.exists? File.join(File.dirname(path), required_file)
    }
  }.each { |path|
    FileUtils.rm_rf File.dirname(path)
  }
end
