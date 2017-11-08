#!/bin/bash

export SUPERUSER_KEY=`sed ':a;N;$!ba;s/\n/\\n/g' {{pkg.svc_config_path}}/pivotal.pem`
export WEBUI_KEY=`sed ':a;N;$!ba;s/\n/\\n/g' {{pkg.svc_config_path}}/webui_priv.pem`

cd {{pkg.path}}/oc-chef-pedant
bundle exec bin/oc-chef-pedant -c {{pkg.svc_config_path}}/pendant_config.rb
