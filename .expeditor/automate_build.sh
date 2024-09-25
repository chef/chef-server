#!/bin/bash -e


# =======================================================CS Things=======================================================
export CHEF_SERVER_SRC='/workdir/src'
export HAB_ORIGIN=cheftest
export HAB_LICENSE=accept-no-persist
# export pkg_name=bookshelf
export PACKAGE_NAME=bookshelf


curl https://raw.githubusercontent.com/habitat-sh/habitat/main/components/hab/install.sh | sudo bash

echo "Is there relevant keys" ` ls -l /var/lib/buildkite-agent/.hab/cache/keys/`

export JOB_TEMP_ROOT
JOB_TEMP_ROOT=$(mktemp -d /tmp/job-root-XXXXXX)
export HAB_CACHE_KEY_PATH
HAB_CACHE_KEY_PATH="$JOB_TEMP_ROOT/keys"

echo "--- :key: Generating fake origin key"
hab license accept
hab origin key generate cheftest

# cd /workdir/src/bookshelf
echo "generating package for $PACKAGE_NAME"
HAB_FEAT_IGNORE_LOCAL=true HAB_ORIGIN=cheftest HAB_CACHE_KEY_PATH=$HAB_CACHE_KEY_PATH DO_CHECK=true hab studio run -D "hab pkg build src/$PACKAGE_NAME"
# echo "which pushd " $(which pushd)


# At this point hart file is in cs results dir.
#What about keys? Line :15
# pushd results
# pkg_name=$(ls -1t *.hart | head -1)
# popd
# echo pkg_name is $pkg_name
# buildkite-agent artifact upload results/$pkg_name

echo "pwd is " `pwd`

# =======================================================Automate Things=======================================================

export OCTOKIT_ACCESS_TOKEN
export HAB_LICENSE=accept
export CHEF_LICENSE="accept-no-persist"
export CI=true
export HAB_ORIGIN=cheftest
export HAB_ORIGIN_KEYS=cheftest
export HAB_STUDIO_SECRET_HAB_FEAT_IGNORE_LOCAL=true
export HAB_FEAT_IGNORE_LOCAL=true


git clone https://github.com/chef/automate.git
cd automate
# git checkout vikas/cs-changes-for-pipeline
git checkout kalroy/cs_plan_changes

cp ../results/*bookshelf*.hart results/

export HAB_STUDIO_HOST_ARCH=x86_64-linux
echo "automate results directory contents" `ls -l results`

Bookself_hart_file=$(ls results/*bookshelf*.hart)
echo "Found hart file: $Bookself_hart_file"
base_name=$(basename "$Bookself_hart_file") # Get just the filename
IFS='-' read -r name comp version timestamp os <<< "${base_name%.hart}"
formatted_output="$comp/$version/$timestamp"

# plan_file="components/automate-cs-bookshelf/habitat/plan.sh"
# sed -i "s|\${vendor_origin}/bookshelf|\${vendor_origin}/${formatted_output}|g" "$plan_file"
# echo "Replaced line in $plan_file"
# sed -i "s|pkg_origin=\"chef\"|pkg_origin=\"cheftest\"|g" "$plan_file"
# # sed -i "s|pkg_origin="chef"|pkg_origin="cheftest"|g" "$plan_file"
# sed -i "s|vendor_origin=\"chef\"|vendor_origin=\"cheftest\"|g" "$plan_file"
# cat components/automate-cs-bookshelf/habitat/plan.sh
# ./scripts/verify_build.sh

# curl https://raw.githubusercontent.com/habitat-sh/habitat/main/components/hab/install.sh | sudo bash

# export JOB_TEMP_ROOT
# JOB_TEMP_ROOT=$(mktemp -d /tmp/job-root-XXXXXX)
# export HAB_CACHE_KEY_PATH
# # HAB_CACHE_KEY_PATH="$JOB_TEMP_ROOT/keys"

# HAB_CACHE_KEY_PATH=$RESOLVED_RESULTS_DIR hab origin key generate cheftest

# hab license accept
# hab origin key generate cheftest

echo "This is the current dir: " `pwd`

echo "Building the package"

echo "contents of place where key should exist" `ls -l $HAB_CACHE_KEY_PATH`

cp $HAB_CACHE_KEY_PATH/* results/

RESOLVED_RESULTS_DIR=$(realpath results/)
HAB_CACHE_KEY_PATH=$RESOLVED_RESULTS_DIR 
HAB_FEAT_OFFLINE_INSTALL=true
ls $HAB_CACHE_KEY_PATH

output_string_vikas=$(echo "$Bookself_hart_file" | sed 's|results/||')
DO_CHECK=true
echo "hab pkg install results/$output_string_vikas"
# hab pkg install results/$output_string_vikas

HAB_FEAT_OFFLINE_INSTALL=true HAB_FEAT_IGNORE_LOCAL=true HAB_ORIGIN=cheftest HAB_CACHE_KEY_PATH=$RESOLVED_RESULTS_DIR DO_CHECK=true HAB_BLDR_CHANNEL=dev hab studio run -D "source .studiorc; set -e; hab pkg install results/$output_string_vikas; hab pkg build components/automate-cs-bookshelf"

# hab studio enter

# echo "Building the package"
# hab build  results/$Bookself_hart_file
# exit


tar -cvf results.tar results
echo "results.tar created"
gzip results.tar

buildkite-agent artifact upload results.tar.gz
