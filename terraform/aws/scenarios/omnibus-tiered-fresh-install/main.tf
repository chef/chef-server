module "back_end" {
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
  name              = "backend-${var.scenario}-${var.enable_ipv6 ? "ipv6" : "ipv4"}-${var.platform}"
}

module "front_end" {
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
  name              = "frontend-${var.scenario}-${var.enable_ipv6 ? "ipv6" : "ipv4"}-${var.platform}"
}

# generate static hosts configuration
data "template_file" "hosts_config" {
  template = "${file("${path.module}/templates/hosts.tpl")}"

  vars {
    back_end_ip  = "${var.enable_ipv6 == true ? module.back_end.public_ipv6_address : module.back_end.private_ipv4_address}"
    front_end_ip = "${var.enable_ipv6 == true ? module.front_end.public_ipv6_address : module.front_end.private_ipv4_address}"
  }
}

# generate chef-server.rb configuration
data "template_file" "chef_server_config" {
  template = "${file("${path.module}/templates/chef-server.rb.tpl")}"

  vars {
    enable_ipv6  = "${var.enable_ipv6}"
    back_end_ip  = "${var.enable_ipv6 == "true" ? module.back_end.public_ipv6_address : module.back_end.private_ipv4_address}"
    front_end_ip = "${var.enable_ipv6 == "true" ? module.front_end.public_ipv6_address : module.front_end.private_ipv4_address}"
    cidr         = "${var.enable_ipv6 == "true" ? 64 : 32}"
  }
}

# update back-end chef server
resource "null_resource" "back_end_config" {
  # provide some connection info
  connection {
    type = "ssh"
    user = "${module.back_end.ssh_username}"
    host = "${module.back_end.public_ipv4_dns}"
  }

  provisioner "file" {
    content     = "${data.template_file.hosts_config.rendered}"
    destination = "/tmp/hosts"
  }

  provisioner "file" {
    content     = "${data.template_file.chef_server_config.rendered}"
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
      "curl -vo /tmp/${replace(var.upgrade_version_url, "/^.*\\//", "")} ${var.upgrade_version_url}",
      "sudo ${replace(var.upgrade_version_url, "rpm", "") != var.upgrade_version_url ? "rpm -U" : "dpkg -iEG"} /tmp/${replace(var.upgrade_version_url, "/^.*\\//", "")}",
      "sudo chown root:root /tmp/chef-server.rb",
      "sudo chown root:root /tmp/dhparam.pem",
      "sudo chown root:root /tmp/hosts",
      "sudo mv /tmp/chef-server.rb /etc/opscode",
      "sudo mv /tmp/dhparam.pem /etc/opscode",
      "sudo mv /tmp/hosts /etc/hosts",
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

  # copy configuration to front-end
  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "sudo tar -C /etc -czf /tmp/opscode.tgz opscode",
      "scp -o 'UserKnownHostsFile=/dev/null' -o 'StrictHostKeyChecking=no' /tmp/opscode.tgz ${module.back_end.ssh_username}@${module.front_end.public_ipv4_dns}:/tmp",
    ]
  }
}

# update front-end chef server
resource "null_resource" "front_end_config" {
  depends_on = ["null_resource.back_end_config"]

  # provide some connection info
  connection {
    type = "ssh"
    user = "${module.front_end.ssh_username}"
    host = "${module.front_end.public_ipv4_dns}"
  }

  provisioner "file" {
    content     = "${data.template_file.hosts_config.rendered}"
    destination = "/tmp/hosts"
  }

  # install chef-server
  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "sudo chown root:root /tmp/hosts",
      "sudo mv /tmp/hosts /etc/hosts",
      "sudo tar -C /etc -xzf /tmp/opscode.tgz",
      "curl -vo /tmp/${replace(var.upgrade_version_url, "/^.*\\//", "")} ${var.upgrade_version_url}",
      "sudo ${replace(var.upgrade_version_url, "rpm", "") != var.upgrade_version_url ? "rpm -U" : "dpkg -iEG"} /tmp/${replace(var.upgrade_version_url, "/^.*\\//", "")}",
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
