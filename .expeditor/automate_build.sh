#!/bin/bash -e

echo "inside script"
git clone https://github.com/chef/automate.git
cd automate
git checkout vikas/cs-changes-for-pipeline
pushd results
buildkite-agent artifact download "chef-*.hart"
popd

echo "results directory contents" `ls -l results`
./scripts/verify_build.sh

tar -cvf results.tar results

gzip results.tar

buildkite-agent artifact upload results.tar.gz
