name 'cleanup'
default_version '1.0.0'
skip_transitive_dependency_licensing true
license :project_license

build do
  # Remove relatively large, unused modules from the core erlang
  # installation
  delete "#{install_dir}/embedded/lib/erlang/lib/megaco-*"
  delete "#{install_dir}/embedded/lib/erlang/lib/wx-*"
  # strip gecode shared object files related to gecode installs
  command "strip #{install_dir}/embedded/lib/libgecode*.so.32.0"
  command "strip #{install_dir}/embedded/lib/ruby/gems/2.2.0/gems/dep-selector-libgecode-*/lib/dep-selector-libgecode/vendored-gecode/lib/*.so"
end
