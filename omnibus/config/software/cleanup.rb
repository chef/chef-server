name 'cleanup'
default_version '1.0.0'
skip_transitive_dependency_licensing true
license :project_license

build do
  # Delete cached .gem files and git checkouts
  delete "#{install_dir}/embedded/lib/ruby/gems/2.*/cache/*.gem"
  delete "#{install_dir}/embedded/service/gem/ruby/2.*/cache/*.gem"
  # Remove nodejs
  delete "#{install_dir}/embedded/nodejs"
  # strip shared object files related to gecode installs
  command "strip #{install_dir}/embedded/lib/libgecode*.so.32.0"

  # remove any test fixture pivotal keys to avoid user confusion
  command "find #{install_dir} -name pivotal.pem -delete"
end
