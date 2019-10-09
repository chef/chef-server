module "chef_server" {
  source = "../../aws_instance"

  aws_profile       = "${var.aws_profile}"
  aws_region        = "${var.aws_region}"
  aws_vpc_name      = "${var.aws_vpc_name}"
  aws_department    = "${var.aws_department}"
  aws_contact       = "${var.aws_contact}"
  aws_ssh_key_id    = "${var.aws_ssh_key_id}"
  aws_instance_type = "${var.aws_instance_type}"
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

  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "curl -vo /tmp/chef-server-stable.rpm https://packages.chef.io/files/stable/chef-server/${var.stable_version}/el/7/chef-server-core-${var.stable_version}-1.el7.x86_64.rpm",
      "sudo rpm -U /tmp/chef-server-stable.rpm",
      "sudo chown root:root /tmp/chef-server.rb",
      "sudo chown root:root /tmp/dhparam.pem",
      "sudo mv /tmp/chef-server.rb /etc/opscode",
      "sudo mv /tmp/dhparam.pem /etc/opscode",
      "sudo chef-server-ctl reconfigure --chef-license=accept",
      "sleep 120",
      "sudo chef-server-ctl user-create janedoe Jane Doe janed@example.com abc123 --filename /tmp/janedoe.pem",
      "sudo chef-server-ctl org-create 4thcoffee 'Fourth Coffee, Inc.' --association_user janedoe --filename /tmp/4thcoffee-validator.pem",
      "curl -vo /tmp/chef-server-unstable.rpm https://packages.chef.io/files/unstable/chef-server/${var.unstable_version}/el/7/chef-server-core-${var.unstable_version}-1.el7.x86_64.rpm",
      "sudo rpm -U /tmp/chef-server-unstable.rpm",
      "sudo CHEF_LICENSE='accept' chef-server-ctl upgrade",
      "sudo chef-server-ctl start",
      "sudo chef-server-ctl cleanup",
      "sleep 120",
      "sudo chef-server-ctl test -J pedant.xml --all --compliance-proxy-tests",
    ]
  }
}
