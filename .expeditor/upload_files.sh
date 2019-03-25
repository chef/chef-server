#!/bin/bash

set -eou pipefail

VERSION=$(cat VERSION)
export VERSION
# Should ensure License Scout doesn't get rate limited
OCTOKIT_ACCESS_TOKEN=$GITHUB_TOKEN
export OCTOKIT_ACCESS_TOKEN

# Generate the License Scout Dependency Manifest
.expeditor/license_scout.sh

# Upload the License Scout Dependency Manifest to the S3 bucket
aws s3 cp chef-server-dependency-licenses.json "s3://chef-automate-artifacts/licenses/chef-server/$VERSION.json" --acl public-read --profile chef-cd
aws s3 cp chef-server-dependency-licenses.json "s3://chef-automate-artifacts/unstable/latest/chef-server/licenses.json" --acl public-read --profile chef-cd

#
# Generate the manifest.json
#

# Download or create the versions file
aws s3 cp "s3://chef-automate-artifacts/unstable/latest/chef-server/versions.json" existing-versions.json --profile chef-cd || echo "[]" > existing-versions.json

# Use create_manifest.rb to generate the manifest.json file
ruby .expeditor/create_manifest.rb

# Append the new version to the unstable channel versions file
jq ". |= .+ [\"$VERSION\"]" existing-versions.json > updated-versions.json

# Upload the manifest to the S3 bucket
aws s3 cp "$VERSION.json" "s3://chef-automate-artifacts/manifests/chef-server/$VERSION.json" --acl public-read --profile chef-cd
aws s3 cp "$VERSION.json" "s3://chef-automate-artifacts/unstable/latest/chef-server/manifest.json" --acl public-read --profile chef-cd
aws s3 cp updated-versions.json "s3://chef-automate-artifacts/unstable/latest/chef-server/versions.json" --acl public-read --profile chef-cd

#
# Cleanup
#

rm "$VERSION.json"
rm existing-versions.json
rm updated-versions.json
