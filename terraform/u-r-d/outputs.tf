output "chef_server_fqdn" {
  value = "${module.chef_server_omnibus_standalone_inplace_upgrade_server.fqdn}"
}
