module "chef_server" {
  source = "../../modules/aws_instance"

  aws_profile       = "${var.aws_profile}"
  aws_region        = "${var.aws_region}"
  aws_vpc_name      = "${var.aws_vpc_name}"
  aws_department    = "${var.aws_department}"
  aws_contact       = "${var.aws_contact}"
  aws_ssh_key_id    = "${var.aws_ssh_key_id}"
  aws_instance_type = "${var.aws_instance_type}"
  enable_ipv6       = "${var.enable_ipv6}"
  platform          = "${var.platform}"
  name              = "chefserver-${var.scenario}-${var.enable_ipv6 ? "ipv6" : "ipv4"}-${var.platform}"
}

module "ldap" {
  source = "../../modules/aws_instance"

  aws_profile       = "${var.aws_profile}"
  aws_region        = "${var.aws_region}"
  aws_vpc_name      = "${var.aws_vpc_name}"
  aws_department    = "${var.aws_department}"
  aws_contact       = "${var.aws_contact}"
  aws_ssh_key_id    = "${var.aws_ssh_key_id}"
  aws_instance_type = "${var.aws_instance_type}"
  enable_ipv6       = "${var.enable_ipv6}"
  platform          = "ubuntu-16.04"
  name              = "ldap-${var.scenario}-${var.enable_ipv6 ? "ipv6" : "ipv4"}-${var.platform}"
}

# generate static hosts configuration
data "template_file" "hosts_config" {
  template = "${file("${path.module}/templates/hosts.tpl")}"

  vars {
    chef_server_ip = "${var.enable_ipv6 == true ? module.chef_server.public_ipv6_address : module.chef_server.private_ipv4_address}"
    ldap_ip        = "${var.enable_ipv6 == true ? module.ldap.public_ipv6_address : module.ldap.private_ipv4_address}"
  }
}

# update ldap server
resource "null_resource" "ldap_config" {
  # provide some connection info
  connection {
    type = "ssh"
    user = "${module.ldap.ssh_username}"
    host = "${module.ldap.public_ipv4_dns}"
  }

  provisioner "file" {
    content     = "${data.template_file.hosts_config.rendered}"
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
  depends_on = ["null_resource.ldap_config"]

  provisioner "local-exec" {
    command = "chef-run --user ${module.ldap.ssh_username} ${module.ldap.public_ipv4_dns} ${path.module}/../../../../dev/cookbooks/provisioning/recipes/ldap-server.rb"
  }
}

# update chef server
resource "null_resource" "chef_server_config" {
  depends_on = ["null_resource.ldap_cookbook"]

  # provide some connection info
  connection {
    type = "ssh"
    user = "${module.chef_server.ssh_username}"
    host = "${module.chef_server.public_ipv4_dns}"
  }

  provisioner "file" {
    content     = "${data.template_file.hosts_config.rendered}"
    destination = "/tmp/hosts"
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
      "sudo chown root:root /tmp/hosts",
      "sudo mv /tmp/hosts /etc/hosts",
      "curl -vo /tmp/${replace(var.upgrade_version_url, "/^.*\\//", "")} ${var.upgrade_version_url}",
      "sudo ${replace(var.upgrade_version_url, "rpm", "") != var.upgrade_version_url ? "rpm -U" : "dpkg -iEG"} /tmp/${replace(var.upgrade_version_url, "/^.*\\//", "")}",
      "sudo chown root:root /tmp/chef-server.rb",
      "sudo chown root:root /tmp/dhparam.pem",
      "sudo mv /tmp/chef-server.rb /etc/opscode",
      "sudo mv /tmp/dhparam.pem /etc/opscode",
      "sudo chef-server-ctl reconfigure --chef-license=accept",
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

  # install push jobs and run pedant test
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
