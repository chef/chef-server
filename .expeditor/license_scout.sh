#!/bin/bash

set -eou pipefail

if [[ "${EXPEDITOR:-false}" == "true" ]]; then
  apt-get update
  apt-get install -y libpq-dev libsqlite3-dev
fi

bundle_install_dirs=(
  chef-server-ctl
  oc-id
  opscode-expander
)

for dir in "${bundle_install_dirs[@]}"; do
  echo "--- Installing gem dependencies for $dir"
  pushd "src/$dir"
    if [[ "${EXPEDITOR:-false}" == "true" ]]; then
      "$(hab pkg path core/ruby25)"/bin/bundle install
    else
      bundle install
    fi
  popd
done

echo "+++ Running License Scout"
license_scout --only-show-failures
