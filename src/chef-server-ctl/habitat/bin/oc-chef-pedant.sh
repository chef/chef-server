#!/bin/bash -e

export SUPERUSER_KEY=`sed ':a;N;$!ba;s/\n/\\n/g' /hab/svc/chef-server-ctl/config/pivotal.pem`
export WEBUI_KEY=`sed ':a;N;$!ba;s/\n/\\n/g' /hab/svc/chef-server-ctl/config/webui_priv.pem`

pkg_prefix=__PKG_PATH__
cd "$pkg_prefix/oc-chef-pedant"

export GEM_HOME="$pkg_prefix/vendor/bundle"
__RUBY_PATH__/bin/bundle exec bin/oc-chef-pedant --log-file /dev/null -c /hab/svc/chef-server-ctl/config/pedant_config.rb --focus smoke
