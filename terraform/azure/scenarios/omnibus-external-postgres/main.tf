module "chef_server" {
  source = "../../modules/arm_instance"

  arm_tenant_id           = "${var.arm_tenant_id}"
  arm_subscription_id     = "${var.arm_subscription_id}"
  arm_location            = "${var.arm_location}"
  arm_resource_group_name = "${var.arm_resource_group_name}"
  arm_department          = "${var.arm_department}"
  arm_contact             = "${var.arm_contact}"
  arm_ssh_key_file        = "${var.arm_ssh_key_file}"
  arm_instance_type       = "${var.arm_instance_type}"
  platform                = "${var.platform}"
  name                    = "chefserver-${var.scenario}-${replace(var.platform, ".", "")}"
}

resource "azurerm_postgresql_server" "default" {
  resource_group_name = "${module.chef_server.resource_group_name}"
  location            = "${module.chef_server.location}"

  name                         = "${var.scenario}-${replace(var.platform, ".", "")}"
  administrator_login          = "bofh"
  administrator_login_password = "i1uvd3v0ps!"
  version                      = "9.6"
  ssl_enforcement              = "Enabled"

  sku {
    name     = "B_Gen5_2"
    capacity = 2
    tier     = "Basic"
    family   = "Gen5"
  }

  storage_profile {
    storage_mb            = 5120
    backup_retention_days = 7
    geo_redundant_backup  = "Disabled"
  }

  tags = {
    X-Dept    = "${var.arm_department}"
    X-Contact = "${var.arm_contact}"
  }
}

resource "azurerm_postgresql_firewall_rule" "default" {
  depends_on = ["module.chef_server"]

  resource_group_name = "${module.chef_server.resource_group_name}"
  server_name         = "${azurerm_postgresql_server.default.name}"

  name             = "${var.scenario}-${replace(var.platform, ".", "")}"
  start_ip_address = "${module.chef_server.public_ipv4_address}"
  end_ip_address   = "${module.chef_server.public_ipv4_address}"
}

# generate chef-server.rb configuration
data "template_file" "chef_server_config" {
  depends_on = ["azurerm_postgresql_server.default"]

  template = "${file("${path.module}/templates/chef-server.rb.tpl")}"

  vars {
    postgres_fqdn = "${azurerm_postgresql_server.default.fqdn}"
  }
}

resource "null_resource" "chef_server_config" {
  depends_on = ["azurerm_postgresql_firewall_rule.default"]

  # provide some connection info
  connection {
    type = "ssh"
    user = "${module.chef_server.ssh_username}"
    host = "${module.chef_server.public_ipv4_address}"
  }

  provisioner "file" {
    content     = "${data.template_file.chef_server_config.rendered}"
    destination = "/tmp/chef-server.rb"
  }

  provisioner "file" {
    source      = "${path.module}/../../../common/files/dhparam.pem"
    destination = "/tmp/dhparam.pem"
  }

  # install chef-server
  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "echo -e '\nBEGIN INSTALL CHEF SERVER\n'",
      "curl -vo /tmp/${replace(var.upgrade_version_url, "/^.*\\//", "")} ${var.upgrade_version_url}",
      "sudo ${replace(var.upgrade_version_url, "rpm", "") != var.upgrade_version_url ? "rpm -U" : "dpkg -iEG"} /tmp/${replace(var.upgrade_version_url, "/^.*\\//", "")}",
      "sudo chown root:root /tmp/chef-server.rb",
      "sudo chown root:root /tmp/dhparam.pem",
      "sudo mv /tmp/chef-server.rb /etc/opscode",
      "sudo mv /tmp/dhparam.pem /etc/opscode",
      "sudo chef-server-ctl reconfigure --chef-license=accept",
      "sleep 120",
      "echo -e '\nEND INSTALL CHEF SERVER\n'",
    ]
  }

  # run smoke test
  provisioner "remote-exec" {
    script = "${path.module}/../../../common/files/test_chef_server-smoke.sh"
  }

  # install push jobs addon
  provisioner "remote-exec" {
    script = "${path.module}/../../../common/files/install_addon_push_jobs.sh"
  }

  # test push jobs addon
  provisioner "remote-exec" {
    script = "${path.module}/../../../common/files/test_addon_push_jobs.sh"
  }

  # install chef manage addon
  provisioner "remote-exec" {
    script = "${path.module}/../../../common/files/install_addon_chef_manage.sh"
  }

  # run pedant test
  provisioner "remote-exec" {
    script = "${path.module}/../../../common/files/test_chef_server-pedant.sh"
  }
}
