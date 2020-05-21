module "chef_server" {
  source = "../../modules/aws_instance"

  aws_profile       = var.aws_profile
  aws_region        = var.aws_region
  aws_vpc_name      = var.aws_vpc_name
  aws_department    = var.aws_department
  aws_contact       = var.aws_contact
  aws_ssh_key_id    = var.aws_ssh_key_id
  aws_instance_type = var.aws_instance_type
  enable_ipv6       = var.enable_ipv6
  platform          = var.platform
  build_prefix      = var.build_prefix
  capture_paths     = ["/etc/opscode", "/var/log/opscode"]
  name              = "chefserver-${var.scenario}-${var.enable_ipv6 == "true" ? "ipv6" : "ipv4"}-${var.platform}"
}

module "ldap" {
  source = "../../modules/aws_instance"

  aws_profile       = var.aws_profile
  aws_region        = var.aws_region
  aws_vpc_name      = var.aws_vpc_name
  aws_department    = var.aws_department
  aws_contact       = var.aws_contact
  aws_ssh_key_id    = var.aws_ssh_key_id
  aws_instance_type = var.aws_instance_type
  enable_ipv6       = var.enable_ipv6
  platform          = "ubuntu-16.04"
  build_prefix      = var.build_prefix
  name              = "ldap-${var.scenario}-${var.enable_ipv6 == "true" ? "ipv6" : "ipv4"}-${var.platform}"
}

# generate static hosts configuration
data "template_file" "hosts_config" {
  template = file("${path.module}/templates/hosts.tpl")

  vars = {
    chef_server_ip = var.enable_ipv6 == "true" ? module.chef_server.public_ipv6_address : module.chef_server.private_ipv4_address
    # TODO(ssd) 2020-05-21: As far as I can tell, the erlang eldap
    # library has a bug in it that prevents us from connecting to IPv6
    # LDAP servers.
    ldap_ip        = module.ldap.private_ipv4_address
  }
}

# update ldap server
resource "null_resource" "ldap_config" {
  # provide some connection info
  connection {
    type = "ssh"
    user = module.ldap.ssh_username
    host = module.ldap.public_ipv4_dns
  }

  provisioner "file" {
    content     = data.template_file.hosts_config.rendered
    destination = "/tmp/hosts"
  }

  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "sudo chown root:root /tmp/hosts",
      "sudo mv /tmp/hosts /etc/hosts",
    ]
  }
}

# install/configure ldap service
resource "null_resource" "ldap_cookbook" {
  depends_on = [null_resource.ldap_config]

  provisioner "local-exec" {
    command = "chef-run --user ${module.ldap.ssh_username} ${module.ldap.public_ipv4_dns} ${path.module}/../../../../dev/cookbooks/provisioning/recipes/ldap-server.rb"
  }
}

# update chef server
resource "null_resource" "chef_server_config" {
  depends_on = [null_resource.ldap_cookbook]

  # provide some connection info
  connection {
    type = "ssh"
    user = module.chef_server.ssh_username
    host = module.chef_server.public_ipv4_dns
  }

  provisioner "file" {
    content     = data.template_file.hosts_config.rendered
    destination = "/tmp/hosts"
  }

  provisioner "file" {
    source      = "${path.module}/files/chef-server.rb"
    destination = "/tmp/chef-server.rb"
  }

  provisioner "file" {
    source      = "${path.module}/../../../common/files/dhparam.pem"
    destination = "/tmp/dhparam.pem"
  }

  provisioner "file" {
    source      = "${path.module}/files/pedant_config.rb"
    destination = "/tmp/pedant_config.rb"
  }

  provisioner "file" {
    source      = "${path.module}/../../../common/files/install_addon_chef_manage.sh"
    destination = "/tmp/install_addon_chef_manage.sh"
  }

  # install chef-server
  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "echo -e '\nBEGIN INSTALL CHEF SERVER\n'",
      "sudo chown root:root /tmp/hosts",
      "sudo mv /tmp/hosts /etc/hosts",
      "curl -vo /tmp/${replace(var.upgrade_version_url, "/^.*\\//", "")} ${var.upgrade_version_url}",
      "sudo ${replace(var.upgrade_version_url, "rpm", "") != var.upgrade_version_url ? "rpm -U" : "dpkg -iEG"} /tmp/${replace(var.upgrade_version_url, "/^.*\\//", "")}",
      "sudo chown root:root /tmp/chef-server.rb",
      "sudo chown root:root /tmp/dhparam.pem",
      "sudo mv /tmp/chef-server.rb /etc/opscode",
      "sudo mv /tmp/dhparam.pem /etc/opscode",
      "cat /tmp/pedant_config.rb | sudo tee -a /opt/opscode/embedded/cookbooks/private-chef/templates/default/pedant_config.rb.erb",
      "sudo chef-server-ctl reconfigure --chef-license=accept",
      "sleep 120",
      "echo -e '\nEND INSTALL CHEF SERVER\n'",
    ]
  }

  # install chef manage addon
  provisioner "remote-exec" {
    inline = [
      "chmod +x /tmp/install_addon_chef_manage.sh",
      "ENABLE_ADDON_CHEF_MANAGE=${var.enable_addon_chef_manage} /tmp/install_addon_chef_manage.sh",
    ]
  }

  # add user + organization
  provisioner "remote-exec" {
    script = "${path.module}/../../../common/files/add_user.sh"
  }
}

resource "null_resource" "chef_server_test" {
  depends_on = [null_resource.chef_server_config]

  connection {
    type = "ssh"
    user = module.chef_server.ssh_username
    host = module.chef_server.public_ipv4_dns
  }

  provisioner "file" {
    source      = "${path.module}/../../../common/files/test_psql.sh"
    destination = "/tmp/test_psql.sh"
  }

  provisioner "file" {
    source      = "${path.module}/../../../common/files/test_gather_logs.sh"
    destination = "/tmp/test_gather_logs.sh"
  }

  # run LDAP smoke test
  #
  # NOTE(ssd) 2020-05-21: It appears not all of our tests really work
  # well with LDAP set up.
  provisioner "remote-exec" {
    inline = [
      "sudo chef-server-ctl test -J pedant.xml --focus ldap",
    ]
  }

  # run psql test
  provisioner "remote-exec" {
    inline = [
      "chmod +x /tmp/test_psql.sh",
      "ENABLE_PSQL_TEST=${var.enable_psql_test} /tmp/test_psql.sh",
    ]
  }

  # run gather-logs test
  provisioner "remote-exec" {
    inline = [
      "chmod +x /tmp/test_gather_logs.sh",
      "ENABLE_GATHER_LOGS_TEST=${var.enable_gather_logs_test} /tmp/test_gather_logs.sh",
    ]
  }
}
