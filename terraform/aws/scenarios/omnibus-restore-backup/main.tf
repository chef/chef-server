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
  name              = "${var.scenario}-${var.enable_ipv6 == "true" ? "ipv6" : "ipv4"}-${var.platform}"
}

resource "null_resource" "chef_server_config" {
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
      "echo -e '\nBEGIN INSTALL CHEF SERVER\n'",
      "curl -vo /tmp/${replace(var.upgrade_version_url, "/^.*\\//", "")} ${var.upgrade_version_url}",
      "sudo ${replace(var.upgrade_version_url, "rpm", "") != var.upgrade_version_url ? "rpm -U" : "dpkg -iEG"} /tmp/${replace(var.upgrade_version_url, "/^.*\\//", "")}",
      "sudo chown root:root /tmp/chef-server.rb",
      "sudo chown root:root /tmp/dhparam.pem",
      "sudo mv /tmp/chef-server.rb /etc/opscode",
      "sudo mv /tmp/dhparam.pem /etc/opscode",
      "export CHEF_LICENSE=accept",
      "sudo chef-server-ctl reconfigure",
      "sleep 120",
      "echo -e '\nEND INSTALL CHEF SERVER\n'",
    ]
  }

  # Copy across existing backup. This is very dependent on matching the version of chef-server the backup
  # was generated from.
  provisioner "file" {
    source      = var.backup_location
    destination = "/tmp/chef-backup.tgz"
  }

  provisioner "remote-exec" {
    inline = [
      "echo -e '\nBEGIN RESTORE BACKUP\n'",
      "sudo chef-server-ctl restore /tmp/chef-backup.tgz",
      "echo -e '\nEND RESTORE BACKUP\n'",
    ]
  }
}
