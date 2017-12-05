#!/bin/bash -e

export SUPERUSER_KEY=`sed ':a;N;$!ba;s/\n/\\n/g' /hab/svc/chef-server-ctl/config/pivotal.pem`
export WEBUI_KEY=`sed ':a;N;$!ba;s/\n/\\n/g' /hab/svc/chef-server-ctl/config/webui_priv.pem`

# This uses a config file to find ourselves (and not hardcode our own package name)
# Could do relative to $0, but that can be messy sometimes
cd $(cat /hab/svc/chef-server-ctl/config/pkg_path)/oc-chef-pedant
$(hab pkg path "core/bundler")/bin/bundle exec bin/oc-chef-pedant --log-file /dev/null -c /hab/svc/chef-server-ctl/config/pedant_config.rb --focus smoke
