#!/bin/bash -e

# this script builds all the essential habitat packages for running Chef Server
# additionaly, it exports them as a local docker image
# export CHEF_SERVER_SRC='/src/src'
export ORIGIN=chef
export HAB_LICENSE=accept-no-persist

curl https://raw.githubusercontent.com/habitat-sh/habitat/main/components/hab/install.sh | sudo bash

export JOB_TEMP_ROOT
JOB_TEMP_ROOT=$(mktemp -d /tmp/job-root-XXXXXX)
export HAB_CACHE_KEY_PATH
HAB_CACHE_KEY_PATH="$JOB_TEMP_ROOT/keys"

echo "--- :key: Generating fake origin key"
hab license accept
hab origin key generate
hab pkg build src/bookshelf


# for dir in oc-id openresty-noroot nginx bookshelf chef-server-ctl oc_bifrost oc_erchef; do
#   cd $CHEF_SERVER_SRC/$dir
#   echo "hab version is :" `hab --version`
#   echo "[STATUS] building $dir"
#   build > /var/log/build-${dir}-$(date +%s).log
#   if [[ $dir =~ dbdpg|openresty-noroot ]]; then continue; fi
#   echo "[STATUS] exporting $dir pkg to docker daemon"
#   hab pkg export docker -i "$ORIGIN/{{pkg_name}}" \
#     --no-push-image \
#     --no-tag-latest \
#     --no-tag-version \
#     --no-tag-version-release \
#     --tag-custom "localdev" \
#     $(ls -1t results/*.hart | head -1)
# done
