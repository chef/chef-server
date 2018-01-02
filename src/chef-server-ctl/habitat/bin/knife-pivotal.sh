#!/bin/bash -e

# This uses a config file to find ourselves (and not hardcode our own package name)
# Could do relative to $0, but that can be messy sometimes
cd $(cat /hab/svc/chef-server-ctl/config/pkg_path)/chef
$(hab pkg path "core/bundler")/bin/bundle exec bin/knife $@ -c /hab/svc/chef-server-ctl/config/pivotal.rb
