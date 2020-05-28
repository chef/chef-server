#!/bin/bash -e

pkg_path=__PKG_PATH__
cd "$pkg_path/chef"
export GEM_PATH="${pkg_path}/vendor/bundle"

__RUBY_PATH__/bin/bundle exec bin/knife $@ -c /hab/svc/chef-server-ctl/config/pivotal.rb
