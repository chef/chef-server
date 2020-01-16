#!/bin/bash

set -e

echo "Removing postgresql-9.3"
apt-get --purge remove -y postgresql-9.3

echo "Removing packages.microsoft.com"
# TODO(ssd) 2019-12-13: packages.microsoft.com periodically fails with
# an error such as:
#
# E: Failed to fetch
# https://packages.microsoft.com/ubuntu/18.04/prod/dists/bionic/main/binary-amd64/Packages.gz
# File has unexpected size (93512 != 165979). Mirror sync in progress?
# [IP: 13.74.252.37 443]
#    Hashes of expected file:
#     - Filesize:165979 [weak]
#     - SHA256:179eb71f2afb4a72bf5b11180b4d4c9ccf1644076dd75f5a7bbf880ecefafbba
#     - SHA1:381a8321619083a4063fa8381bf3aa12a2dac5a3 [weak]
#     - MD5Sum:54c730dd6a33c612b2ae3c23fe0cfcb7 [weak]
#    Release file created at: Thu, 12 Dec 2019 19:59:19 +0000
# E: Some index files failed to download. They have been ignored, or old ones used instead.
#
# Since we don't use any software from this repository in our tests,
# we can temporarily remove it from our sources.
rm /etc/apt/sources.list.d/microsoft-prod.list

echo  "Installing test dependencies"
apt-get update -y
apt-get install -y lua5.1 luarocks postgresql-9.6 libsqlite3-dev

echo "Configuring postgresql"
cp /workdir/scripts/bk_tests/pb_hba.conf /etc/postgresql/9.6/main/pg_hba.conf

# the erlang software definition lives in: /omnibus-software/config/software/erlang.rb

# this is needed until the erlang version is installed in the docker container
echo "Installing erlang 22.2"
asdf install erlang 22.2
asdf local erlang 22.2
erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell

echo "Installing Bundler"
gem install bundler --version '~> 1.17' --no-document

echo "Installing Lua"
export LUALIB=~/.luarocks/lib/lua/5.2
luarocks install --local lpeg
luarocks install --local lua-cjson
eval $(luarocks path)

echo "Installing sqitch"
cpanm --notest --quiet --local-lib=$HOME/perl5 local::lib && eval $(perl -I ~/perl5/lib/perl5/ -Mlocal::lib)
cpanm --notest --quiet App::Sqitch

echo "Printing Environment"
env

echo "Restarting Postgresql"
service postgresql restart
