module "back_end" {
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
  name              = "backend-${var.scenario}-${var.enable_ipv6 ? "ipv6" : "ipv4"}-${var.platform}"
}

module "front_end" {
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
  name              = "frontend-${var.scenario}-${var.enable_ipv6 ? "ipv6" : "ipv4"}-${var.platform}"
}

# generate static hosts configuration
data "template_file" "hosts_config" {
  template = file("${path.module}/templates/hosts.tpl")

  vars = {
    back_end_ip  = var.enable_ipv6 == "true" ? module.back_end.public_ipv6_address : module.back_end.private_ipv4_address
    front_end_ip = var.enable_ipv6 == "true" ? module.front_end.public_ipv6_address : module.front_end.private_ipv4_address
    back_end_node_fqdn  = var.enable_ipv6 == "true" ? module.back_end.public_ipv6_address : module.back_end.private_ipv4_dns
    front_end_node_fqdn = var.enable_ipv6 == "true" ? module.front_end.public_ipv6_address : module.front_end.private_ipv4_dns
  }
}

# generate chef-server.rb configuration
data "template_file" "chef_server_config" {
  template = file("${path.module}/templates/chef-server.rb.tpl")

  vars = {
    enable_ipv6  = var.enable_ipv6
    back_end_ip  = var.enable_ipv6 == "true" ? module.back_end.public_ipv6_address : module.back_end.private_ipv4_address
    front_end_ip = var.enable_ipv6 == "true" ? module.front_end.public_ipv6_address : module.front_end.private_ipv4_address
    back_end_node_fqdn  = var.enable_ipv6 == "true" ? module.back_end.public_ipv6_address : module.back_end.private_ipv4_dns
    front_end_node_fqdn = var.enable_ipv6 == "true" ? module.front_end.public_ipv6_address : module.front_end.private_ipv4_dns
    cidr         = var.enable_ipv6 == "true" ? 64 : 32
  }
}

# update back-end chef server
resource "null_resource" "back_end_config" {
  # provide some connection info
  connection {
    type = "ssh"
    user = module.back_end.ssh_username
    host = module.back_end.public_ipv4_dns
  }

  provisioner "file" {
    content     = data.template_file.hosts_config.rendered
    destination = "/tmp/hosts"
  }

  provisioner "file" {
    content     = data.template_file.chef_server_config.rendered
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
      "echo -e '\nBEGIN INSTALL CHEF SERVER (BACK-END)\n'",
      "curl -vo /tmp/${replace(var.install_version_url, "/^.*\\//", "")} ${var.install_version_url}",
      "sudo ${replace(var.install_version_url, "rpm", "") != var.install_version_url ? "rpm -U" : "dpkg -iEG"} /tmp/${replace(var.install_version_url, "/^.*\\//", "")}",
      "sudo chown root:root /tmp/chef-server.rb",
      "sudo chown root:root /tmp/dhparam.pem",
      "sudo chown root:root /tmp/hosts",
      "sudo mv /tmp/chef-server.rb /etc/opscode",
      "sudo mv /tmp/dhparam.pem /etc/opscode",
      "sudo mv /tmp/hosts /etc/hosts",
      "sudo chef-server-ctl reconfigure --chef-license=accept",
      "sleep 120",
      "echo -e '\nEND INSTALL CHEF SERVER (BACK-END)\n'",
    ]
  }

  # add user + organization
  provisioner "remote-exec" {
    script = "${path.module}/../../../common/files/add_user.sh"
  }

  # copy configuration to front-end
  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "echo -e '\nBEGIN COPY CONFIGURATION TO FRONT-END\n'",
      "sudo tar -C /etc -czf /tmp/opscode.tgz opscode",
      "scp -o 'UserKnownHostsFile=/dev/null' -o 'StrictHostKeyChecking=no' /tmp/opscode.tgz ${module.back_end.ssh_username}@${module.front_end.public_ipv4_dns}:/tmp",
      "echo -e '\nEND COPY CONFIGURATION TO FRONT-END\n'",
    ]
  }
}

# update front-end chef server
resource "null_resource" "front_end_config" {
  depends_on = [null_resource.back_end_config]

  # provide some connection info
  connection {
    type = "ssh"
    user = module.front_end.ssh_username
    host = module.front_end.public_ipv4_dns
  }

  provisioner "file" {
    content     = data.template_file.hosts_config.rendered
    destination = "/tmp/hosts"
  }

  # install chef-server
  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "echo -e '\nBEGIN INSTALL CHEF SERVER (FRONT-END)\n'",
      "sudo chown root:root /tmp/hosts",
      "sudo mv /tmp/hosts /etc/hosts",
      "sudo tar -C /etc -xzf /tmp/opscode.tgz",
      "curl -vo /tmp/${replace(var.install_version_url, "/^.*\\//", "")} ${var.install_version_url}",
      "sudo ${replace(var.install_version_url, "rpm", "") != var.install_version_url ? "rpm -U" : "dpkg -iEG"} /tmp/${replace(var.install_version_url, "/^.*\\//", "")}",
      "sudo chef-server-ctl reconfigure --chef-license=accept",
      "sleep 120",
      "echo -e '\nEND INSTALL CHEF SERVER (FRONT-END)\n'",
    ]
  }
}

# upgrade back-end chef server
resource "null_resource" "back_end_upgrade" {
  depends_on = [null_resource.front_end_config]

  # provide some connection info
  connection {
    type = "ssh"
    user = module.back_end.ssh_username
    host = module.back_end.public_ipv4_dns
  }

  # upgrade chef-server
  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "echo -e '\nBEGIN UPGRADE CHEF SERVER (BACK-END)\n'",
      "curl -vo /tmp/${replace(var.upgrade_version_url, "/^.*\\//", "")} ${var.upgrade_version_url}",
      "sudo ${replace(var.upgrade_version_url, "rpm", "") != var.upgrade_version_url ? "rpm -U" : "dpkg -iEG"} /tmp/${replace(var.upgrade_version_url, "/^.*\\//", "")}",
      "sudo CHEF_LICENSE='accept' chef-server-ctl upgrade",
      "sudo chef-server-ctl start",
      "sudo chef-server-ctl cleanup",
      "sleep 120",
      "echo -e '\nEND UPGRADE CHEF SERVER (BACK-END)\n'",
    ]
  }
}

# upgrade front-end chef server
resource "null_resource" "front_end_upgrade" {
  depends_on = [null_resource.back_end_upgrade]

  # provide some connection info
  connection {
    type = "ssh"
    user = module.front_end.ssh_username
    host = module.front_end.public_ipv4_dns
  }

  # upgrade chef-server
  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "echo -e '\nBEGIN UPGRADE CHEF SERVER (FRONT-END)\n'",
      "curl -vo /tmp/${replace(var.upgrade_version_url, "/^.*\\//", "")} ${var.upgrade_version_url}",
      "sudo ${replace(var.upgrade_version_url, "rpm", "") != var.upgrade_version_url ? "rpm -U" : "dpkg -iEG"} /tmp/${replace(var.upgrade_version_url, "/^.*\\//", "")}",
      "sudo CHEF_LICENSE='accept' chef-server-ctl upgrade",
      "sudo chef-server-ctl start",
      "sudo chef-server-ctl cleanup",
      "sleep 30",
      "echo -e '\nEND UPGRADE CHEF SERVER (FRONT-END)\n'",
    ]
  }
}

resource "null_resource" "chef_server_test" {
  depends_on = [null_resource.front_end_upgrade]

  connection {
    type = "ssh"
    user = module.front_end.ssh_username
    host = module.front_end.public_ipv4_dns
  }

  # upload test scripts
  provisioner "file" {
    source      = "${path.module}/../../../common/files/test_chef_server-smoke.sh"
    destination = "/tmp/test_chef_server-smoke.sh"
  }

  provisioner "file" {
    source      = "${path.module}/../../../common/files/install_addon_chef_manage.sh"
    destination = "/tmp/install_addon_chef_manage.sh"
  }

  provisioner "file" {
    source      = "${path.module}/../../../common/files/test_chef_server-pedant.sh"
    destination = "/tmp/test_chef_server-pedant.sh"
  }

  provisioner "file" {
    source      = "${path.module}/../../../common/files/test_psql.sh"
    destination = "/tmp/test_psql.sh"
  }

  provisioner "file" {
    source      = "${path.module}/../../../common/files/test_gather_logs.sh"
    destination = "/tmp/test_gather_logs.sh"
  }

  # run smoke test
  provisioner "remote-exec" {
    inline = [
      "chmod +x /tmp/test_chef_server-smoke.sh",
      "ENABLE_SMOKE_TEST=${var.enable_smoke_test} /tmp/test_chef_server-smoke.sh",
    ]
  }

  # install + test chef manage addon
  provisioner "remote-exec" {
    inline = [
      "chmod +x /tmp/install_addon_chef_manage.sh",
      "ENABLE_ADDON_CHEF_MANAGE=${var.enable_addon_chef_manage} /tmp/install_addon_chef_manage.sh",
    ]
  }

  # run pedant test
  provisioner "remote-exec" {
    inline = [
      "chmod +x /tmp/test_chef_server-pedant.sh",
      "ENABLE_PEDANT_TEST=${var.enable_pedant_test} /tmp/test_chef_server-pedant.sh",
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
resource "null_resource" "chef_server_install_push" {
  depends_on = [null_resource.back_end_config]

  connection {
    type = "ssh"
    user = module.back_end.ssh_username
    host = module.back_end.public_ipv4_dns
   }

   provisioner "file" {
    source      = "${path.module}/../../../common/files/install_addon_push_jobs.sh"
    destination = "/tmp/install_addon_push_jobs.sh"
   }

   provisioner "file" {
    source      = "${path.module}/../../../common/files/test_addon_push_jobs.sh"
    destination = "/tmp/test_addon_push_jobs.sh"
   }

   # install + test push jobs addon
   provisioner "remote-exec" {
     inline = [
       "chmod +x /tmp/install_addon_push_jobs.sh",
       "ENABLE_ADDON_PUSH_JOBS=${var.enable_addon_push_jobs} /tmp/install_addon_push_jobs.sh",
       "chmod +x /tmp/test_addon_push_jobs.sh",
       "ENABLE_ADDON_PUSH_JOBS=${var.enable_addon_push_jobs} /tmp/test_addon_push_jobs.sh",
     ]
   }
}
