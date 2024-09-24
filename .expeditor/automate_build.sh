#!/bin/bash -e

git clone https://github.com/chef/automate.git
cd automate
git checkout vikas/cs-changes-for-pipeline

# buildkite-agent artifact download "*.hart" ./results

# echo "results directory contents" `ls -l results`
export HAB_NONINTERACTIVE=true
export HAB_STUDIO_SECRET_HAB_NONINTERACTIVE=true
export HAB_NOCOLORING=true
export HAB_LICENSE="accept-no-persist"
export HAB_ORIGIN=chef
export HAB_STUDIO_SECRET_HAB_FEAT_IGNORE_LOCAL=true
export HAB_STUDIO_SECRET_HAB_FEAT_OFFLINE_INSTALL=true
export ALLOW_LOCAL_PACKAGES=true
hab license accept
RESOLVED_RESULTS_DIR=$(realpath results/)

JOB_TEMP_ROOT=$(mktemp -d /tmp/job-root-XXXXXX)
export JOB_TEMP_ROOT

echo "generate ephemeral origin key"
#HAB_CACHE_KEY_PATH="$JOB_TEMP_ROOT/keys"
HAB_CACHE_KEY_PATH=$RESOLVED_RESULTS_DIR hab origin key generate chef
export HAB_CACHE_KEY_PATH

# Build oc_erchef_pkg
pushd results
echo "generating package for oc_erchef"
hab pkg build "../../src/oc_erchef"
echo "which pushd " $(which pushd)

pkg_name=$(ls -1t *.hart | head -1)
popd

echo $pkg_name
pwd
../../.expeditor/replace.sh

# hab studio run -D "source .studiorc; set -e; env; hab pkg install results/<built hart file name of oc_erchef>; build components/automate-cs-oc-erchef"
hab studio run -D "source .studiorc; set -e; env; hab pkg install $pkg_name; build components/automate-cs-oc-erchef"

# hab studio run -D "source .studiorc; set -e; build components/automate-cs-bookshelf"

# hab studio run -D "source .studiorc; set -e; build components/automate-cs-nginx"

# hab studio run -D "source .studiorc; set -e; build components/automate-cs-oc-bifrost"

# hab studio run -D "source .studiorc; set -e; build components/automate-cs-oc-erchef"

# hab studio run -D "source .studiorc; set -e; build components/automate-cs-ocid"

echo "after build" `ls -l results`