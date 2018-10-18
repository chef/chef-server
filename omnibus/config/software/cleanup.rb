name 'cleanup'
default_version '1.0.0'
skip_transitive_dependency_licensing true
license :project_license

build do
  # strip shared object files related to gecode installs
  command "strip #{install_dir}/embedded/lib/libgecode*.so.32.0"

  # remove any test fixture pivotal keys to avoid user confusion
  command "find #{install_dir} -name pivotal.pem -delete"
end
