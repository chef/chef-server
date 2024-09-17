export CHEF_SERVER_SRC='/workdir/src'
export ORIGIN=chef
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
hab pkg build src/$PACKAGE_NAME
pushd results
pkg_name=$(ls -1t *.hart | head -1)
popd
echo pkg_name is $pkg_name
buildkite-agent artifact upload results/$pkg_name