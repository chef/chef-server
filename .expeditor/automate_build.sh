#!/bin/bash -e

git clone https://github.com/chef/automate.git
cd automate
git checkout vikas/cs-changes-for-pipeline

# buildkite-agent artifact download "*.hart" ./results


# echo "results directory contents" `ls -l results`
export HAB_NONINTERACTIVE=true
export HAB_STUDIO_SECRET_HAB_NONINTERACTIVE=true
export HAB_NOCOLORING=true
export HAB_STUDIO_SECRET_HAB_FEAT_IGNORE_LOCAL=false
export HAB_STUDIO_SECRET_HAB_FEAT_OFFLINE_INSTALL=true
export HAB_LICENSE="accept-no-persist"
export HAB_ORIGIN=chef
export ALLOW_LOCAL_PACKAGES=true

export JOB_TEMP_ROOT
JOB_TEMP_ROOT=$(mktemp -d /tmp/job-root-XXXXXX)
export HAB_CACHE_KEY_PATH
HAB_CACHE_KEY_PATH="$JOB_TEMP_ROOT/keys"

echo "--- :key: Generating fake origin key"
hab license accept
hab origin key generate

hab studio run -D "source .studiorc; set -e; build components/automate-cs-bookshelf"

echo "after build" `ls -l results`
./scripts/verify_build.sh

tar -cvf results.tar results
echo "results.tar created"
gzip results.tar

buildkite-agent artifact upload results.tar.gz