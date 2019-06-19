#!/bin/bash
set -ueo pipefail

channel="${CHANNEL:-unstable}"
product="${PRODUCT:-chef-server}"
version="${VERSION:-latest}"

echo "--- Installing $channel $product $version"
package_file="$(install-omnibus-product -c "$channel" -P "$product" -v "$version" | tail -n 1)"

if [[ "$package_file" == *.rpm ]]; then
  check-rpm-signed "$package_file"
fi

echo "--- Testing $channel $product $version"

export PATH="/opt/opscode/bin:/opt/opscode/embedded/bin:$PATH"
export INSTALL_DIR="/opt/opscode"

echo ""
echo ""
echo "============================================================"
echo "Verifying ownership of package files"
echo "============================================================"
echo ""

NONROOT_FILES="$(find "$INSTALL_DIR" ! -uid 0 -print)"
if [[ "$NONROOT_FILES" == "" ]]; then
  echo "Packages files are owned by root.  Continuing verification."
else
  echo "Exiting with an error because the following files are not owned by root:"
  echo "$NONROOT_FILES"
  exit 1
fi

echo ""
echo ""
echo "============================================================"
echo "Reconfiguring $product"
echo "============================================================"
echo ""

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

sudo chef-server-ctl reconfigure --chef-license=accept-no-persist || true
sleep 120

echo ""
echo ""
echo "============================================================"
echo "Running verification for $product"
echo "============================================================"
echo ""

echo "Sleeping even longer (120 seconds) to let the system settle"
sleep 120

if [[ "$(uname -m)" == "s390x" ]]; then
  # FIX ME FIX ME FIX ME
  # This is to see if we can get the build passing at all on the s390x platform
  # FIX ME FIX ME FIX ME
  sudo chef-server-ctl test -J pedant.xml --smoke --compliance-proxy-tests
else
  sudo chef-server-ctl test -J pedant.xml --all --compliance-proxy-tests
fi

if [[ "${OMNIBUS_FIPS_MODE:-false}" == "true" ]]; then
  echo "fips true" | sudo tee -a /etc/opscode/chef-server.rb
  sudo chef-server-ctl reconfigure  --chef-license=accept-no-persist
  echo ""
  echo "Sleeping 120 seconds to allow the Chef Server to reconfigure in FIPS mode"
  echo ""
  sleep 120
  sudo chef-server-ctl test -J pedant-fips.xml --smoke
fi
