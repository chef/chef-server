#!/bin/bash -e

git clone https://github.com/chef/automate.git
cd automate
git checkout vikas/cs-changes-for-pipeline

# buildkite-agent artifact download "*.hart" ./results

../.expeditor/replace.sh
./scripts/verify_build.sh

# # echo "results directory contents" `ls -l results`
# export HAB_NONINTERACTIVE=true
# export HAB_STUDIO_SECRET_HAB_NONINTERACTIVE=true
# export HAB_NOCOLORING=true
# export HAB_LICENSE="accept-no-persist"
# export HAB_ORIGIN=chef
# export HAB_STUDIO_SECRET_HAB_FEAT_IGNORE_LOCAL=true
# export HAB_STUDIO_SECRET_HAB_FEAT_OFFLINE_INSTALL=true
# export ALLOW_LOCAL_PACKAGES=true

# export JOB_TEMP_ROOT
# JOB_TEMP_ROOT=$(mktemp -d /tmp/job-root-XXXXXX)
# export HAB_CACHE_KEY_PATH
# HAB_CACHE_KEY_PATH="$JOB_TEMP_ROOT/keys"

# echo "--- :key: Generating fake origin key"
# hab license accept
# hab origin key generate

# hab studio run -D "source .studiorc; set -e; build "components/automate-cs-oc-erchef"

# # hab studio run -D "source .studiorc; set -e; build components/automate-cs-bookshelf"

# # hab studio run -D "source .studiorc; set -e; build components/automate-cs-nginx"

# # hab studio run -D "source .studiorc; set -e; build components/automate-cs-oc-bifrost"

# # hab studio run -D "source .studiorc; set -e; build components/automate-cs-oc-erchef"

# # hab studio run -D "source .studiorc; set -e; build components/automate-cs-ocid"

# echo "after build" `ls -l results`