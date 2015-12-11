#!/bin/bash

# run this script as root
# this script assumes you are starting with a VM with Chef Server Latest installed
# and external DBs set up

# REQUIRED ENV VARIABLES
# CHEF_SERVER_LATEST: path to last release of the chef server .deb build
# CHEF_SERVER_LATEST_MINUS_1: path to a N-1 release of the chef server .deb build
# CHEF_SERVER_11: path to an open source server 11 .deb build
# ENTERPRISE_CHEF_SERVER_11: path to a private chef .deb build
# REPORTING_LAST_RELEASE: path to .deb of last release of reporting
# REPORTING_LATEST: path to .deb of reporting build you are testing

# Validation

# exit 1 if any of the following exits 1
set -e

if [ -z "$CHEF_SERVER_LATEST" ];
then
    echo "\$CHEF_SERVER_LATEST unset and is required."
    exit 1
fi

if [ -z "$CHEF_SERVER_LATEST_MINUS_1" ];
then
    echo "\$CHEF_SERVER_LATEST_MINUS_1 unset and is required."
    exit 1
fi

if [ -z "$CHEF_SERVER_11" ];
then
    echo "\$CHEF_SERVER_11 unset and is required."
    exit 1
fi

if [ -z "$ENTERPRISE_CHEF_SERVER_11" ];
then
    echo "\$ENTERPRISE_CHEF_SERVER_11 unset and is required."
    exit 1
fi

if [ -z "$REPORTING_LAST_RELEASE" ];
then
    echo "\$REPORTING_LATEST unset and is required."
    exit 1
fi

if [ -z "$REPORTING_LATEST" ];
then
    echo "\$REPORTING_LATEST unset and is required."
    exit 1
fi  

# Write Config Files

# External Server Config 1)

echo "postgresql['db_superuser'] = \"bofh\"" > /tmp/external-postgres-1.rb
echo "postgresql['db_superuser_password'] = \"i1uvd3v0ps\"" >> /tmp/external-postgres-1.rb
echo "postgresql['external'] = true" >> /tmp/external-postgres-1.rb
echo "postgresql['vip'] = \"192.168.33.150\"" >> /tmp/external-postgres-1.rb

# External Server Config 2)

echo "postgresql['db_superuser'] = \"bofh\"" > /tmp/external-postgres-2.rb
echo "postgresql['db_superuser_password'] = \"i1uvd3v0ps\"" >> /tmp/external-postgres-2.rb
echo "postgresql['external'] = true" >> /tmp/external-postgres-2.rb
echo "postgresql['vip'] = \"192.168.33.155\"" >> /tmp/external-postgres-2.rb

echo "Chef Server Latest, Reporting Latest"

dpkg -i $REPORTING_LATEST
chef-server-ctl reconfigure
opscode-reporting-ctl reconfigure

echo "Chef Server Latest, Reporting Latest, Shared Chef Server DB"

opscode-reporting-ctl test --all
chef-server-ctl test

echo "Chef Server Latest, Reporting Latest, Shared External DB"

opscode-reporting-ctl cleanse yes --with-external
chef-server-ctl cleanse yes --with-external
mkdir -p /etc/opscode
mkdir -p /etc/opscode-reporting
cp /tmp/external-postgres-1.rb /etc/opscode/chef-server.rb
cp /tmp/external-postgres-1.rb /etc/opscode-reporting/opscode-reporting.rb
chef-server-ctl reconfigure
opscode-reporting-ctl reconfigure
opscode-reporting-ctl test --all
chef-server-ctl test

echo "Chef Server Latest, Reporting Latest, Independant External DBs"

opscode-reporting-ctl cleanse yes --with-external
chef-server-ctl cleanse yes --with-external
mkdir -p /etc/opscode
mkdir -p /etc/opscode-reporting
cp /tmp/external-postgres-1.rb /etc/opscode/chef-server.rb
cp /tmp/external-postgres-2.rb /etc/opscode-reporting/opscode-reporting.rb
chef-server-ctl reconfigure
opscode-reporting-ctl reconfigure
opscode-reporting-ctl test --all
chef-server-ctl test

echo "Chef Server Latest-1, Reporting Latest"

opscode-reporting-ctl cleanse yes --with-external
chef-server-ctl cleanse yes --with-external
apt-get remove chef-server-core -y
dpkg -i $CHEF_SERVER_LATEST_MINUS_1

echo "Chef Server Latest-1, Reporting Latest, Shared Chef Server DB"

rm -f /etc/opscode/chef-server.rb
rm -f /etc/opscode-reporting/opscode-reporting.rb
chef-server-ctl reconfigure
opscode-reporting-ctl reconfigure
opscode-reporting-ctl test --all
chef-server-ctl test

echo "Chef Server Latest-1, Reporting Latest, Chef Server Internal DB, Reporting External DB"

opscode-reporting-ctl cleanse yes --with-external
chef-server-ctl cleanse yes --with-external
mkdir -p /etc/opscode
mkdir -p /etc/opscode-reporting
cp /tmp/external-postgres-1.rb /etc/opscode-reporting/opscode-reporting.rb
chef-server-ctl reconfigure
opscode-reporting-ctl reconfigure
opscode-reporting-ctl test --all
chef-server-ctl test

echo "Enterprise Chef Server 11 Latest, Reporting Latest"

opscode-reporting-ctl cleanse yes --with-external
chef-server-ctl cleanse yes --with-external
apt-get remove chef-server-core -y
dpkg -i $ENTERPRISE_CHEF_SERVER_11

echo "Enterprise Chef Server 11 Latest, Reporting Latest, Shared Chef Server DB"

rm -f /etc/opscode/chef-server.rb
rm -f /etc/opscode-reporting/opscode-reporting.rb
private-chef-ctl reconfigure
opscode-reporting-ctl reconfigure
opscode-reporting-ctl test --all
private-chef-ctl test

echo "Enterprise Chef Server 11 Latest, Reporting Latest, Internal Chef Server DB, External Reporting DB"

opscode-reporting-ctl cleanse yes --with-external
private-chef-ctl cleanse yes
mkdir -p /etc/opscode
mkdir -p /etc/opscode-reporting
rm -f /etc/opscode/chef-server.rb
cp /tmp/external-postgres-1.rb /etc/opscode-reporting/opscode-reporting.rb
private-chef-ctl reconfigure
opscode-reporting-ctl reconfigure
opscode-reporting-ctl test --all
private-chef-ctl test

echo "(Open Source) Chef Server 11 Latest, Reporting Last Released Shared DB, Upgrade Reporting to Latest"

opscode-reporting-ctl cleanse yes --with-external
private-chef-ctl cleanse yes
mkdir -p /etc/opscode
mkdir -p /etc/opscode-reporting
rm -f /etc/opscode/chef-server.rb
rm -f /etc/opscode-reporting/opscode-reporting.rb
apt-get remove chef-server-core -y
dpkg -i $CHEF_SERVER_11
apt-get remove opscode-reporting -y
dpkg -i $REPORTING_LAST_RELEASE
chef-server-ctl reconfigure
opscode-reporting-ctl reconfigure
opscode-reporting-ctl test --all
chef-server-ctl test
dpkg -i $REPORTING_LATEST
chef-server-ctl reconfigure
opscode-reporting-ctl reconfigure
opscode-reporting-ctl test --all
chef-server-ctl test

echo "Chef Server Latest, Reporting Last Release Shared DB, Upgrade Reporting to Latest"

opscode-reporting-ctl cleanse yes --with-external
chef-server-ctl cleanse yes
mkdir -p /etc/opscode
mkdir -p /etc/opscode-reporting
rm -f /etc/opscode/chef-server.rb
rm -f /etc/opscode-reporting/opscode-reporting.rb
apt-get remove chef-server-core -y
dpkg -i $CHEF_SERVER_LATEST
apt-get remove opscode-reporting -y
dpkg -i $REPORTING_LAST_RELEASE
chef-server-ctl reconfigure
opscode-reporting-ctl reconfigure
opscode-reporting-ctl test --all
chef-server-ctl test
dpkg -i $REPORTING_LATEST
chef-server-ctl reconfigure
opscode-reporting-ctl reconfigure
opscode-reporting-ctl test --all
chef-server-ctl test

