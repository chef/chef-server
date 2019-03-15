#!/bin/bash

set -eou pipefail

if [[ "${EXPEDITOR:-false}" == "true" ]]; then
  apt-get update
  apt-get install -y libpq-dev libsqlite3-dev
  # Pin ruby to 2.5.3 since chef-server tests heavily depend
  asdf local ruby 2.5.3
  # Install gem for 2.5.3 path
  gem install license_scout
fi

bundle_install_dirs=(
  chef-server-ctl
  oc-id
  opscode-expander
)

for dir in "${bundle_install_dirs[@]}"; do
  echo "--- Installing gem dependencies for $dir"
  pushd "src/$dir"
    bundle install
  popd
done

erlang_install_dirs=(
  bookshelf
  chef-mover
  oc_bifrost
  oc_erchef
)

for dir in "${erlang_install_dirs[@]}"; do
  echo "--- Installing rebar dependencies for $dir"
  pushd "src/$dir"
    ./rebar3 get-deps
  popd
done

echo "+++ Running License Scout"
license_scout --only-show-failures
