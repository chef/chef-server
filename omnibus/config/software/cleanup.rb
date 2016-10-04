name 'cleanup'
default_version '1.0.0'
skip_transitive_dependency_licensing true
license :project_license

build do
  # Delete cached .gem files and git checkouts
  delete "#{install_dir}/embedded/lib/ruby/gems/2.2.0/cache/*.gem"
  delete "#{install_dir}/embedded/service/gem/ruby/2.2.0/cache/*.gem"
  # strip gecode shared object files related to gecode installs
  command "strip #{install_dir}/embedded/lib/libgecode*.so.32.0"
  command "strip #{install_dir}/embedded/lib/ruby/gems/2.2.0/gems/dep-selector-libgecode-*/lib/dep-selector-libgecode/vendored-gecode/lib/*.so"
end
