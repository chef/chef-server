#!/bin/bash

export SUPERUSER_KEY=`sed ':a;N;$!ba;s/\n/\\n/g' /hab/svc/chef-server-ctl/config/pivotal.pem`
export WEBUI_KEY=`sed ':a;N;$!ba;s/\n/\\n/g' /hab/svc/chef-server-ctl/config/webui_priv.pem`

cd $(hab pkg path "chef-server/chef-server-ctl")/oc-chef-pedant
$(hab pkg path "core/bundler")/bin/bundle exec bin/oc-chef-pedant --log-file /dev/null -c /hab/svc/chef-server-ctl/config/pendant_config.rb --focus smoke
