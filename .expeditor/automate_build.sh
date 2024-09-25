#!/bin/bash -e

export ORIGIN=chef
export HAB_ORIGIN=chef
export HAB_LICENSE=accept-no-persist
export OCTOKIT_ACCESS_TOKEN
export HAB_LICENSE=accept
export CHEF_LICENSE="accept-no-persist"
export CI=true
#export HAB_ORIGIN=cheftest
export HAB_ORIGIN=chef
#export HAB_ORIGIN_KEYS=cheftest
export HAB_ORIGIN_KEYS=chef
export HAB_STUDIO_SECRET_HAB_FEAT_IGNORE_LOCAL=false
export HAB_FEAT_IGNORE_LOCAL=false
export HAB_STUDIO_HOST_ARCH=x86_64-linux
export HAB_FEAT_OFFLINE_INSTALL=true

curl https://raw.githubusercontent.com/habitat-sh/habitat/main/components/hab/install.sh | sudo bash

export JOB_TEMP_ROOT
JOB_TEMP_ROOT=$(mktemp -d /tmp/job-root-XXXXXX)
export HAB_CACHE_KEY_PATH
HAB_CACHE_KEY_PATH="$JOB_TEMP_ROOT/keys"

echo "--- :key: Generating fake origin key"
hab license accept
hab origin key generate

for pkg_name in `echo "bookshelf chef-server-ctl oc-id oc_bifrost oc_erchef openresty-noroot"`
do
  echo "generating package for $pkg_name"
  hab pkg build "src/$pkg_name"
done

./.expeditor/replace.sh "nginx" "src"

echo "generating package for nginx"
openresty_hart=$(ls -1t results/chef-openresty*.hart | head -1)
HAB_FEAT_OFFLINE_INSTALL=true HAB_FEAT_IGNORE_LOCAL=false HAB_ORIGIN=chef HAB_CACHE_KEY_PATH="$JOB_TEMP_ROOT/keys" DO_CHECK=true HAB_BLDR_CHANNEL=dev hab studio run -D "set -e; hab pkg install $openresty_hart; hab pkg build src/nginx"

cp $HAB_CACHE_KEY_PATH/* results
tar -cvf results.tar results
gzip results.tar
buildkite-agent artifact upload results.tar.gz

git clone https://github.com/chef/automate.git
cd automate
git checkout vikas/cs-changes-for-pipeline

RESOLVED_RESULTS_DIR=$(realpath results/)
export DO_CHECK=true

cp ../results/* results
../.expeditor/replace.sh
bookshelf_hart=$(ls -1t results/chef-bookshelf*.hart | head -1)
chef_server_ctl_hart=$(ls -1t results/chef-chef-server-ctl*.hart | head -1)
nginx=$(ls -1t results/chef-nginx*.hart | head -1)
oc_id=$(ls -1t results/chef-oc_id*.hart | head -1)
bifrost_hart=$(ls -1t results/chef-oc_bifrost*.hart | head -1)
erchef_hart=$(ls -1t results/chef-oc_erchef*.hart | head -1)


HAB_FEAT_OFFLINE_INSTALL=true HAB_FEAT_IGNORE_LOCAL=false HAB_ORIGIN=chef HAB_CACHE_KEY_PATH="$JOB_TEMP_ROOT/keys" DO_CHECK=true HAB_BLDR_CHANNEL=dev hab studio run -D "set -e; hab pkg install $bookshelf_hart; hab pkg build components/automate-cs-bookshelf"

HAB_FEAT_OFFLINE_INSTALL=true HAB_FEAT_IGNORE_LOCAL=false HAB_ORIGIN=chef HAB_CACHE_KEY_PATH="$JOB_TEMP_ROOT/keys" DO_CHECK=true HAB_BLDR_CHANNEL=dev hab studio run -D "set -e; hab pkg install $bifrost_hart; hab pkg build components/automate-cs-oc-bifrost"

HAB_FEAT_OFFLINE_INSTALL=true HAB_FEAT_IGNORE_LOCAL=false HAB_ORIGIN=chef HAB_CACHE_KEY_PATH="$JOB_TEMP_ROOT/keys" DO_CHECK=true HAB_BLDR_CHANNEL=dev hab studio run -D "set -e; hab pkg install $erchef_hart; hab pkg build components/automate-cs-oc-erchef"

HAB_FEAT_OFFLINE_INSTALL=true HAB_FEAT_IGNORE_LOCAL=false HAB_ORIGIN=chef HAB_CACHE_KEY_PATH="$JOB_TEMP_ROOT/keys" DO_CHECK=true HAB_BLDR_CHANNEL=dev hab studio run -D "set -e; hab pkg install $oc_id; hab pkg build components/automate-cs-ocid"

HAB_FEAT_OFFLINE_INSTALL=true HAB_FEAT_IGNORE_LOCAL=false HAB_ORIGIN=chef HAB_CACHE_KEY_PATH="$JOB_TEMP_ROOT/keys" DO_CHECK=true HAB_BLDR_CHANNEL=dev hab studio run -D "set -e; hab pkg install $nginx; hab pkg build components/automate-cs-nginx"

hab studio run -D "source .studiorc; set -e; env; hab pkg install $bookshelf_hart; build components/automate-cs-bookshelf"

echo "after build" `ls -l results`

tar -cvf results1.tar results
gzip results1.tar
buildkite-agent artifact upload results1.tar.gz