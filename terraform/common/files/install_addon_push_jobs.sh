#!/bin/bash

set -evx

echo -e '\nBEGIN INSTALL PUSH JOBS ADDON\n'

sudo chef-server-ctl install opscode-push-jobs-server
sudo chef-server-ctl reconfigure --chef-license=accept
sleep 30
sudo opscode-push-jobs-server-ctl reconfigure
sleep 30

echo -e '\nEND INSTALL PUSH JOBS ADDON\n'
