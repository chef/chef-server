#!/bin/bash

export LANG=en_US.UTF-8

set -e
set -x

echo "Attempting to remove private-chef, forcing success in case it's not installed"

# Check whether a command exists - returns 0 if it does, 1 if it does not
exists() {
  if command -v $1 &>/dev/null
  then
    return 0
  else
    return 1
  fi
}

if exists dpkg;
then
  sudo dpkg -P private-chef || true
  sudo jenkins/opc-killer.sh
  sudo dpkg -i pkg/private-chef*deb
else
  sudo rpm -ev private-chef || true
  sudo jenkins/opc-killer.sh
  sudo rpm -Uvh pkg/private-chef*rpm
fi

export PATH=/opt/opscode/bin:/opt/opscode/embedded/bin:$PATH
sudo private-chef-ctl reconfigure
sleep 120
sudo private-chef-ctl test -J $WORKSPACE/pedant.xml
# when build succeeds, nuke the packages
find . -type d -maxdepth 1 -mindepth 1 | xargs rm -rf
