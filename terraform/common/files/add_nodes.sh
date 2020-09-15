#!/bin/bash

set -evx

echo -e '\nBEGIN ADD NODES\n'

echo -e '\nBEGIN INSTALL HAB\n'
curl https://raw.githubusercontent.com/habitat-sh/habitat/master/components/hab/install.sh | sudo -E bash

echo -e '\nBEGIN INSTALL HAB\n'
hab license accept

echo -e '\nBEGIN INSTALL and BINLINK chef-load\n'
sudo hab pkg install chef/chef-load -bf

echo -e '\nBEGIN NODE GENERATION \n'
sudo chef-load generate --config /tmp/chef-load.toml -n 100 --node_name_prefix chef-node --threads 5

sudo chef-server-ctl psql oc_erchef <<<"SELECT count(*) FROM nodes; \q"

echo -e '\nEND ADD NODES\n'
