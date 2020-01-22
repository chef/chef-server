#!/bin/bash

set -evx

ret=0

echo -e '\nBEGIN TEST CHEF BACKEND DEMOTION\n'

echo 'Original Chef Backend leader status'
sudo chef-backend-ctl cluster-status | tee /tmp/chef-backend-status.orig

echo 'Demoting current leader'
sudo chef-backend-ctl demote >/dev/null 2>&1
sleep 30

echo 'Current Chef Backend leader status'
sudo chef-backend-ctl cluster-status | tee /tmp/chef-backend-status.cur

leaders=$(sudo awk '/leader.*leader/{print $3}' /tmp/chef-backend-status.{orig,cur} | sort -u | wc -l)

if [[ $leaders -ne 2 ]]; then
    ret=1

    echo 'ERROR: Leadership transfer was not successful'

    sudo diff /tmp/chef-backend-status.{orig,cur}
fi

echo -e '\nEND TEST CHEF BACKEND DEMOTION\n'

exit $ret
