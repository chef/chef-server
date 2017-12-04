#!/bin/bash -e

cd $(hab pkg path "chef-server/chef-server-ctl")/chef
$(hab pkg path "core/bundler")/bin/bundle exec bin/knife $@ -c /hab/svc/chef-server-ctl/config/pivotal.rb
