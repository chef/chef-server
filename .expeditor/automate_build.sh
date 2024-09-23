#!/bin/bash -e

echo "inside script"
git clone https://github.com/chef/automate.git
cd automate
git checkout vikas/cs-changes-for-pipeline
# pushd results
# buildkite-agent artifact download "*.hart" ./
# popd

echo "results directory contents" `ls -l results`

Bookself_hart_file=$(ls results/*bookshelf*.hart)
echo "Found hart file: $Bookself_hart_file"
base_name=$(basename "$Bookself_hart_file") # Get just the filename
IFS='-' read -r name comp version timestamp os <<< "${base_name%.hart}"
formatted_output="$comp/$version/$timestamp"

plan_file="components/automate-cs-bookshelf/habitat/plan.sh"
sed -i "s|\${vendor_origin}/bookshelf|\$cheftest/${formatted_output}|g" "$plan_file"
echo "Replaced line in $plan_file"
cat components/automate-cs-bookshelf/habitat/plan.sh
./scripts/verify_build.sh

tar -cvf results.tar results
echo "results.tar created"
gzip results.tar

buildkite-agent artifact upload results.tar.gz
