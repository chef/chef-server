output "resource_group_name" {
  value = "${data.azurerm_resource_group.chef_resource_group.name}"
}

output "location" {
  value = "${data.azurerm_resource_group.chef_resource_group.location}"
}

output "public_ipv4_address" {
  value = "${azurerm_public_ip.default.ip_address}"
}

output "private_ipv4_address" {
  value = "${azurerm_network_interface.default.private_ip_address}"
}

output "ssh_username" {
  value = "azure"
}
