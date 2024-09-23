#!/bin/bash -e

# this script builds all the essential habitat packages for running Chef Server
# additionaly, it exports them as a local docker image

export CHEF_SERVER_SRC='/workdir/src'
export ORIGIN=cheftest
export HAB_LICENSE=accept-no-persist
# export PACKAGE_NAME=$1


curl https://raw.githubusercontent.com/habitat-sh/habitat/main/components/hab/install.sh | sudo bash

export JOB_TEMP_ROOT
JOB_TEMP_ROOT=$(mktemp -d /tmp/job-root-XXXXXX)
export HAB_CACHE_KEY_PATH
HAB_CACHE_KEY_PATH="$JOB_TEMP_ROOT/keys"

echo "--- :key: Generating fake origin key"
hab license accept
hab origin key generate

# cd /workdir/src/bookshelf
echo "generating package for $PACKAGE_NAME"
hab pkg build "src/$PACKAGE_NAME"
echo "which pushd " $(which pushd)

pushd results
pkg_name=$(ls -1t *.hart | head -1)
popd
echo pkg_name is $pkg_name
buildkite-agent artifact upload results/$pkg_name