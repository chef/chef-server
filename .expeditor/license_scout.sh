#!/bin/bash

set -eou pipefail

if [[ "${BUILDKITE:-false}" == "true" ]]; then
  # TODO(ssd) 2019-12-13: packages.microsoft.com periodically fails with
  # an error such as:
  #
  # E: Failed to fetch
  # https://packages.microsoft.com/ubuntu/18.04/prod/dists/bionic/main/binary-amd64/Packages.gz
  # File has unexpected size (93512 != 165979). Mirror sync in progress?
  # [IP: 13.74.252.37 443]
  #    Hashes of expected file:
  #     - Filesize:165979 [weak]
  #     - SHA256:179eb71f2afb4a72bf5b11180b4d4c9ccf1644076dd75f5a7bbf880ecefafbba
  #     - SHA1:381a8321619083a4063fa8381bf3aa12a2dac5a3 [weak]
  #     - MD5Sum:54c730dd6a33c612b2ae3c23fe0cfcb7 [weak]
  #    Release file created at: Thu, 12 Dec 2019 19:59:19 +0000
  # E: Some index files failed to download. They have been ignored, or old ones used instead.
  #
  # Since we don't use any software from this repository in our tests,
  # we can temporarily remove it from our sources.
  rm /etc/apt/sources.list.d/microsoft-prod.list
  apt-get update
  apt-get install -y libpq-dev libsqlite3-dev
fi

bundle_install_dirs=(
  chef-server-ctl
  oc-id
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
