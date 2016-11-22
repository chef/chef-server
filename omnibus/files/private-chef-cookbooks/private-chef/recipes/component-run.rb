include_recipe "private-chef::plugin_discovery"
include_recipe "private-chef::plugin_config_extensions"
include_recipe "private-chef::config"
include_recipe "enterprise::runit"

include_recipe "private-chef::#{node['component']}"
