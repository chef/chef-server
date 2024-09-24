#!/bin/bash -e

export OCTOKIT_ACCESS_TOKEN
export HAB_LICENSE=accept
export CHEF_LICENSE="accept-no-persist"
export CI="true"
export HAB_ORIGIN=cheftest
export HAB_ORIGIN_KEYS=cheftest
export HAB_STUDIO_SECRET_HAB_FEAT_IGNORE_LOCAL="true"
export HAB_FEAT_IGNORE_LOCAL="true"


echo "inside script"
git clone https://github.com/chef/automate.git
cd automate
git checkout vikas/cs-changes-for-pipeline
pushd results
buildkite-agent artifact download "*.hart" ./
popd
export HAB_STUDIO_HOST_ARCH=x86_64-linux
echo "results directory contents" `ls -l results`

Bookself_hart_file=$(ls results/*bookshelf*.hart)
echo "Found hart file: $Bookself_hart_file"
base_name=$(basename "$Bookself_hart_file") # Get just the filename
IFS='-' read -r name comp version timestamp os <<< "${base_name%.hart}"
formatted_output="$comp/$version/$timestamp"

plan_file="components/automate-cs-bookshelf/habitat/plan.sh"
sed -i "s|\${vendor_origin}/bookshelf|\${vendor_origin}/${formatted_output}|g" "$plan_file"
echo "Replaced line in $plan_file"
sed -i "s|pkg_origin=\"chef\"|pkg_origin=\"cheftest\"|g" "$plan_file"
# sed -i "s|pkg_origin="chef"|pkg_origin="cheftest"|g" "$plan_file"
sed -i "s|vendor_origin=\"chef\"|vendor_origin=\"cheftest\"|g" "$plan_file"
cat components/automate-cs-bookshelf/habitat/plan.sh
# ./scripts/verify_build.sh

curl https://raw.githubusercontent.com/habitat-sh/habitat/main/components/hab/install.sh | sudo bash

export JOB_TEMP_ROOT
JOB_TEMP_ROOT=$(mktemp -d /tmp/job-root-XXXXXX)
export HAB_CACHE_KEY_PATH
HAB_CACHE_KEY_PATH="$JOB_TEMP_ROOT/keys"


hab license accept
hab origin key generate cheftest

echo "This is the current dir: " `pwd`

echo "Building the package"


output_string_vikas=$(echo "$Bookself_hart_file" | sed 's|results/||')

hab pkg build results/$output_string_vikas

# hab studio enter

# echo "Building the package"
# hab build  results/$Bookself_hart_file
# exit


tar -cvf results.tar results
echo "results.tar created"
gzip results.tar

buildkite-agent artifact upload results.tar.gz
