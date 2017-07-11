# Render an openssl config template that enables fips
# mode by default. Applications that call OPENSSL_config
# can make use of this, for example nginx
#
# This template will only be used if fips mode is enabled
# in the chef server config
template "/opt/opscode/embedded/ssl/openssl.fips.cnf" do
  source "openssl.fips.cnf.erb"
  owner "root"
  group "root"
  mode "0644"
end
