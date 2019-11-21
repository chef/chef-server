#!/bin/bash

set -evx

echo -e '\nBEGIN INSTALL PUSH JOBS ADDON\n'

sudo chef-server-ctl install opscode-push-jobs-server
sudo mkdir -p /etc/opscode-push-jobs-server
printf -- "opscode_pushy_server['heartbeat_interval'] = 1000" | sudo tee /etc/opscode-push-jobs-server/opscode-push-jobs-server.rb
sudo chef-server-ctl reconfigure --chef-license=accept
sleep 30
sudo opscode-push-jobs-server-ctl reconfigure
sleep 30

echo -e '\nEND INSTALL PUSH JOBS ADDON\n'
