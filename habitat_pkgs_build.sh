#!/bin/bash -e

# this script builds all the essential habitat packages for running Chef Server
# additionaly, it exports them as a local docker image

CHEF_SERVER_SRC='/src/src'

for dir in dbdpg oc-id openresty-noroot nginx bookshelf chef-server-ctl oc_bifrost oc_erchef; do
  cd $CHEF_SERVER_SRC/$dir
  echo "[STATUS] building $dir"
  build > /var/log/build-${dir}-$(date +%s).log
  if [[ $dir =~ dbdpg ]]; then continue; fi
  echo "[STATUS] exporting $dir pkg to docker daemon"
  hab pkg export docker --non-root -i "chefserverofficial/{{pkg_name}}" $(ls -1t results/*.hart | head -1)
done
