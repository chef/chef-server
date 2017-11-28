LIB_PATH="/opt/opscode/embedded/lib"
# The GEM_PATH should work since we allow only one version of ruby to be installed.
GEM_PATH="#{LIB_PATH}/ruby/gems/*/gems"

execute "find #{GEM_PATH} -perm /u=x,g=x,o=x -exec chmod 755 {} \\;" do
  user "root"
end

execute "find #{GEM_PATH} -perm /u=r,g=r,o=r ! -perm /u=x -exec chmod 644 {} \\;" do
  user "root"
end
