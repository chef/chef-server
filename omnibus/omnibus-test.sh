#!/bin/bash
set -ueo pipefail

export PATH="/opt/opscode/bin:/opt/opscode/embedded/bin:$PATH"

echo "--- Reconfiguring chef-server"

sudo mkdir -p /etc/opscode

printf -- "
opscode_erchef['keygen_start_size'] = 30
opscode_erchef['keygen_cache_size']=60
nginx['ssl_dhparam']='/etc/opscode/dhparam.pem'
insecure_addon_compat false
data_collector['token'] = 'foobar'
profiles['root_url'] = 'http://localhost:9998'
" | sudo tee /etc/opscode/chef-server.rb

printf -- "-----BEGIN DH PARAMETERS-----
MIIBCAKCAQEAtAvx3pUHBNcK2nD58nPPlKtJzZvrFCyKEn9BSn16/BmFwBhL8rh4
+fkrnLflZ/k9wJjiUkU0DCi+Fy6DUohPHOmmT0BiuwgsDZAFDyTj0PeZKINpbHnQ
EbZENzWo5s5hsb1zVxIMEtTMRrigdHM3FQupFbzOHxonkO0JlocarOJBHGX+Crjp
y/8SReCpC71R+Vl6d4+Dw6GFdL+6k6W558dPfq3UeV8HPWQEaM7/jXDUKJZ0tB6a
1csrekkz3gBFlSjSxececRVn8bm5dTfc86rIWJWeWQVLYdBFT6zi43AvF+nLYKYh
+oVnVrhWgOLYvEKX311d9SaqcdrXVFscYwIBAg==
-----END DH PARAMETERS-----
" | sudo tee /etc/opscode/dhparam.pem

# At least for now we want to fail if reconfigure fails
sudo chef-server-ctl reconfigure --chef-license=accept-no-persist
sleep 120

echo "--- Running 'chef-server-ctl test'"

echo "Sleeping even longer (120 seconds) to let the system settle"
sleep 120

sudo chef-server-ctl test -J pedant.xml --all --compliance-proxy-tests

if [[ "${OMNIBUS_FIPS_MODE:-false}" == "true" ]]; then
  echo "fips true" | sudo tee -a /etc/opscode/chef-server.rb
  sudo chef-server-ctl reconfigure  --chef-license=accept-no-persist
  echo ""
  echo "Sleeping 120 seconds to allow the Chef Server to reconfigure in FIPS mode"
  echo ""
  sleep 120
  sudo chef-server-ctl test -J pedant-fips.xml --smoke
fi
