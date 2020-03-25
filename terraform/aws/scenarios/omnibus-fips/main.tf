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
  name              = "${var.scenario}-${var.enable_ipv6 ? "ipv6" : "ipv4"}-${var.platform}"
}

resource "null_resource" "chef_server_fips" {
  connection {
    type = "ssh"
    user = module.chef_server.ssh_username
    host = module.chef_server.public_ipv4_dns
  }

  # enable fips mode
  # https://access.redhat.com/documentation/en-us/red_hat_enterprise_linux/7/html/security_guide/chap-federal_standards_and_regulations
  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "echo -e '\nFIPS STATUS:\n'",
      "sudo sysctl crypto.fips_enabled",
      "echo -e '\nBEGIN ENABLING FIPS MODE\n'",
      "sudo yum install -y dracut-fips dracut-fips-aesni",
      "sudo dracut -f",
      "if [ -f /etc/default/grub ]; then sudo sed -i '/GRUB_CMDLINE_LINUX/{s/=\"/=\"fips=1 /;}' /etc/default/grub; sudo grub2-mkconfig -o /boot/grub2/grub.cfg; else sudo sed -i '/^\t.*kernel.*boot/{s/$/ fips=1/;}' /boot/grub/grub.conf; fi",
      "echo -e '\nEND ENABLING FIPS MODE\n'",
    ]
  }

  # reboot instance for fips to take effect
  provisioner "local-exec" {
    command = "AWS_PROFILE=${var.aws_profile} AWS_DEFAULT_REGION=${var.aws_region} aws ec2 reboot-instances --instance-ids ${module.chef_server.id}; sleep 60;"
  }
}

resource "null_resource" "chef_server_config" {
  depends_on = [null_resource.chef_server_fips]

  connection {
    type = "ssh"
    user = module.chef_server.ssh_username
    host = module.chef_server.public_ipv4_dns
  }

  provisioner "file" {
    source      = "${path.module}/files/chef-server.rb"
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
      "echo -e '\nCHECKING FIPS STATUS:\n'",
      "if sudo sysctl crypto.fips_enabled | grep -q 1; then echo 'SUCCESS: FIPS mode is enabled!'; else echo 'FAIL: FIPS mode is not enabled!'; exit 1; fi",
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

  # upload test scripts
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
