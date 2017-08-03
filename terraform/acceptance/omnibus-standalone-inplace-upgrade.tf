module "chef_server_omnibus_standalone_inplace_upgrade_server" {
  source = "git@github.com:chef/es-terraform.git//modules/chef_managed_instance"

  # Global Settings
  environment = "${var.environment}"

  # AWS Settings
  aws_private_key      = "${var.aws_private_key}"
  aws_key_name         = "${var.aws_key_name}"
  aws_root_volume_size = 200

  # This value will be switched to chef-server after a migration
  aws_subdomain = "chef-server-2"

  # AWS Tags
  aws_tag_chef_product      = "chef-server"
  aws_tag_automate_project  = "${var.automate_project}"
  aws_tag_automate_pipeline = "${var.automate_pipeline}"

  # Scenario
  scenario_package_type     = "omnibus"
  scenario_topology         = "standalone"
  scenario_install_strategy = "inplace-upgrade"

  # Chef Provisioner
  chef_environment = "${var.chef_environment}"
  chef_server_url  = "${var.chef_server_url}"
  chef_user_name   = "${var.chef_user_name}"
  chef_user_key    = "${var.chef_user_key}"
}
