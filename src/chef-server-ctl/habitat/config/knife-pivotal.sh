#!/bin/bash

export SUPERUSER_KEY=`sed ':a;N;$!ba;s/\n/\\n/g' {{pkg.svc_config_path}}/pivotal.pem`
export WEBUI_KEY=`sed ':a;N;$!ba;s/\n/\\n/g' {{pkg.svc_config_path}}/webui_priv.pem`

cd {{pkg.path}}/chef
{{pkgPathFor "core/bundler"}}/bin/bundle exec bin/knife $@ -c {{pkg.svc_config_path}}/pivotal.rb
