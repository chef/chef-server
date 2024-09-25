#!/bin/bash -e

export ORIGIN=cheftest
export HAB_ORIGIN=cheftest
export HAB_LICENSE=accept-no-persist

curl https://raw.githubusercontent.com/habitat-sh/habitat/main/components/hab/install.sh | sudo bash

export JOB_TEMP_ROOT
JOB_TEMP_ROOT=$(mktemp -d /tmp/job-root-XXXXXX)
export HAB_CACHE_KEY_PATH
HAB_CACHE_KEY_PATH="$JOB_TEMP_ROOT/keys"

echo "--- :key: Generating fake origin key"
hab license accept
hab origin key generate

# cd /workdir/src/bookshelf
for pkg_name in `echo "bookshelf openresty-noroot"`
do
  echo "generating package for $pkg_name"
  hab pkg build "src/$pkg_name"
done

./.expeditor/replace.sh "nginx" "src"

for pkg_name in `echo "nginx"`
do
  echo "generating package for $pkg_name"
  openresty_hart=$(ls -1t results/chef-openresty*.hart | head -1)
  hab pkg install $openresty_hart
  hab studio run -D "hab pkg install $openresty_hart; build components/automate-cs-bookshelf" "src/$pkg_name"
done

cp $HAB_CACHE_KEY_PATH/* results
tar -cvf results.tar results
gzip results.tar.gz
buildkite-agent artifact upload results.tar.gz

git clone https://github.com/chef/automate.git
cd automate
git checkout vikas/cs-changes-for-pipeline

# buildkite-agent artifact download "*.hart" ./results

# echo "results directory contents" `ls -l results`
export HAB_NONINTERACTIVE=true
export HAB_STUDIO_SECRET_HAB_NONINTERACTIVE=true
export HAB_NOCOLORING=true
export HAB_ORIGIN=chef
export HAB_STUDIO_SECRET_HAB_FEAT_IGNORE_LOCAL=false
export HAB_STUDIO_SECRET_HAB_FEAT_OFFLINE_INSTALL=true
export ALLOW_LOCAL_PACKAGES=true

RESOLVED_RESULTS_DIR=$(realpath results/)
export DO_CHECK=true

cp ../results/* results
../.expeditor/replace.sh
bookshelf_hart=$(ls -1t results/chef-bookshelf*.hart | head -1)

hab studio run -D "source .studiorc; set -e; env; hab pkg install $bookshelf_hart; build components/automate-cs-bookshelf"

# Build oc_erchef_pkg
pushd results
ls ../../src/oc_erchef
echo "generating package for oc_erchef"
hab pkg build ../../src/oc_erchef
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