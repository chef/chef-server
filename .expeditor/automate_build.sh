#!/bin/bash -e

git clone https://github.com/chef/automate.git
cd automate
git checkout vikas/cs-changes-for-pipeline

buildkite-agent artifact download "*.hart" ./results

# echo "results directory contents" `ls -l results`

hab studio run -D "source .studiorc; set -e; build components/automate-cs-bookshelf"

sh -vx ./scripts/verify_build.sh

tar -cvf results.tar results
echo "results.tar created"
gzip results.tar

buildkite-agent artifact upload results.tar.gz