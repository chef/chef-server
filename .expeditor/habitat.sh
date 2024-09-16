#!/bin/bash -e

# this script builds all the essential habitat packages for running Chef Server
# additionaly, it exports them as a local docker image

export CHEF_SERVER_SRC='/src/src'
export ORIGIN=chefservertesting

for dir in oc-id openresty-noroot nginx bookshelf chef-server-ctl oc_bifrost oc_erchef; do
  cd $CHEF_SERVER_SRC/$dir
  echo "[STATUS] building $dir"
  build > /var/log/build-${dir}-$(date +%s).log
  if [[ $dir =~ dbdpg|openresty-noroot ]]; then continue; fi
  echo "[STATUS] exporting $dir pkg to docker daemon"
  hab pkg export docker -i "$ORIGIN/{{pkg_name}}" \
    --no-push-image \
    --no-tag-latest \
    --no-tag-version \
    --no-tag-version-release \
    --tag-custom "localdev" \
    $(ls -1t results/*.hart | head -1)
done
