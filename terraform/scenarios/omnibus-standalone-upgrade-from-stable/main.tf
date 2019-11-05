module "chef_server" {
  source = "../../aws_instance"

  aws_profile       = "${var.aws_profile}"
  aws_region        = "${var.aws_region}"
  aws_vpc_name      = "${var.aws_vpc_name}"
  aws_department    = "${var.aws_department}"
  aws_contact       = "${var.aws_contact}"
  aws_ssh_key_id    = "${var.aws_ssh_key_id}"
  aws_instance_type = "${var.aws_instance_type}"
  enable_ipv6       = "${var.enable_ipv6}"
  platform          = "${var.platform}"
  name              = "${var.scenario}-${var.enable_ipv6 ? "ipv6" : "ipv4"}-${var.platform}"
}

resource "null_resource" "chef_server_config" {
  connection {
    type = "ssh"
    user = "${module.chef_server.ssh_username}"
    host = "${module.chef_server.public_ipv4_dns}"
  }

  provisioner "file" {
    source      = "${path.module}/files/chef-server.rb"
    destination = "/tmp/chef-server.rb"
  }

  provisioner "file" {
    source      = "${path.module}/files/dhparam.pem"
    destination = "/tmp/dhparam.pem"
  }

  # install chef-server
  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "curl -vo /tmp/${replace(var.install_version_url, "/^.*\\//", "")} ${var.install_version_url}",
      "sudo ${replace(var.install_version_url, "rpm", "") != var.install_version_url ? "rpm -U" : "dpkg -iEG"} /tmp/${replace(var.install_version_url, "/^.*\\//", "")}",
      "sudo chown root:root /tmp/chef-server.rb",
      "sudo chown root:root /tmp/dhparam.pem",
      "sudo mv /tmp/chef-server.rb /etc/opscode",
      "sudo mv /tmp/dhparam.pem /etc/opscode",
      "sudo chef-server-ctl reconfigure --chef-license=accept",
      "sleep 30",
    ]
  }

  # add user + organization
  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "sudo chef-server-ctl user-create janedoe Jane Doe janed@example.com abc123 --filename /tmp/janedoe.pem",
      "sudo chef-server-ctl org-create 4thcoffee 'Fourth Coffee, Inc.' --association_user janedoe --filename /tmp/4thcoffee-validator.pem",
    ]
  }

  # upgrade chef-server
  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "curl -vo /tmp/${replace(var.upgrade_version_url, "/^.*\\//", "")} ${var.upgrade_version_url}",
      "sudo ${replace(var.upgrade_version_url, "rpm", "") != var.upgrade_version_url ? "rpm -U" : "dpkg -iEG"} /tmp/${replace(var.upgrade_version_url, "/^.*\\//", "")}",
      "sudo CHEF_LICENSE='accept' chef-server-ctl upgrade",
      "sudo chef-server-ctl start",
      "sudo chef-server-ctl cleanup",
      "sleep 120",
    ]
  }

  # run smoke test
  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "sudo chef-server-ctl test",
    ]
  }

  # install push jobs and run push test
  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "sudo chef-server-ctl install opscode-push-jobs-server",
      "sudo chef-server-ctl reconfigure --chef-license=accept",
      "sleep 30",
      "sudo opscode-push-jobs-server-ctl reconfigure",
      "sleep 30",
      "sudo opscode-push-jobs-server-ctl test",
    ]
  }

  # install chef-manage
  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "sudo chef-server-ctl install chef-manage",
      "sudo chef-server-ctl reconfigure --chef-license=accept",
      "sleep 30",
      "sudo chef-manage-ctl reconfigure --accept-license",
      "sleep 30",
    ]
  }

  # run pedant test
  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "sudo chef-server-ctl test -J pedant.xml --all --compliance-proxy-tests",
    ]
  }
}
