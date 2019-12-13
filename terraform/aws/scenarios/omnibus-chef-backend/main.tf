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

module "backend1" {
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
  name              = "backend1-${var.scenario}-${var.enable_ipv6 ? "ipv6" : "ipv4"}-${var.platform}"
}

module "backend2" {
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
  name              = "backend2-${var.scenario}-${var.enable_ipv6 ? "ipv6" : "ipv4"}-${var.platform}"
}

module "backend3" {
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
  name              = "backend3-${var.scenario}-${var.enable_ipv6 ? "ipv6" : "ipv4"}-${var.platform}"
}

# generate static hosts configuration
data "template_file" "hosts_config" {
  template = "${file("${path.module}/templates/hosts.tpl")}"

  vars {
    chef_server_ip = "${var.enable_ipv6 == true ? module.chef_server.public_ipv6_address : module.chef_server.private_ipv4_address}"
    backend1_ip    = "${var.enable_ipv6 == true ? module.backend1.public_ipv6_address : module.backend1.private_ipv4_address}"
    backend2_ip    = "${var.enable_ipv6 == true ? module.backend2.public_ipv6_address : module.backend2.private_ipv4_address}"
    backend3_ip    = "${var.enable_ipv6 == true ? module.backend3.public_ipv6_address : module.backend3.private_ipv4_address}"
  }
}

# generate chef-backend configuration
data "template_file" "backend_config" {
  template = "${file("${path.module}/templates/chef-backend.rb.tpl")}"

  vars {
    backend1_ip = "${var.enable_ipv6 == true ? module.backend1.public_ipv6_address : module.backend1.private_ipv4_address}"
  }
}

# update backend1 server
resource "null_resource" "backend1_config" {
  # provide some connection info
  connection {
    type = "ssh"
    user = "${module.backend1.ssh_username}"
    host = "${module.backend1.public_ipv4_dns}"
  }

  provisioner "file" {
    content     = "${data.template_file.hosts_config.rendered}"
    destination = "/tmp/hosts"
  }

  provisioner "file" {
    content     = "${data.template_file.backend_config.rendered}"
    destination = "/tmp/chef-backend.rb"
  }

  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "echo -e '\nBEGIN INSTALL CHEF BACKEND1\n'",
      "sudo chown root:root /tmp/hosts",
      "sudo mv /tmp/hosts /etc/hosts",
      "curl -vo /tmp/${replace(var.backend_version_url, "/^.*\\//", "")} ${var.backend_version_url}",
      "sudo ${replace(var.backend_version_url, "rpm", "") != var.backend_version_url ? "rpm -U" : "dpkg -iEG"} /tmp/${replace(var.backend_version_url, "/^.*\\//", "")}",
      "sudo chown root:root /tmp/chef-backend.rb",
      "sudo mv /tmp/chef-backend.rb /etc/chef-backend",
      "sudo chef-backend-ctl create-cluster --accept-license --yes --quiet",
      "sudo cat /etc/chef-backend/chef-backend-secrets.json | ssh -o 'UserKnownHostsFile=/dev/null' -o 'StrictHostKeyChecking=no' ${module.backend2.ssh_username}@${module.backend2.public_ipv4_dns} 'cat > /tmp/chef-backend-secrets.json'",
      "sudo cat /etc/chef-backend/chef-backend-secrets.json | ssh -o 'UserKnownHostsFile=/dev/null' -o 'StrictHostKeyChecking=no' ${module.backend3.ssh_username}@${module.backend3.public_ipv4_dns} 'cat > /tmp/chef-backend-secrets.json'",
      "sudo chef-backend-ctl gen-server-config chefserver.internal > /tmp/chef-server.rb",
      "echo -e \"\nprofiles['root_url'] = 'http://chefserver.internal:9998'\" | sudo tee -a /tmp/chef-server.rb",
      "scp -o 'UserKnownHostsFile=/dev/null' -o 'StrictHostKeyChecking=no' /tmp/chef-server.rb ${module.chef_server.ssh_username}@${module.chef_server.public_ipv4_dns}:/tmp",
      "echo -e '\nEND INSTALL CHEF BACKEND1\n'",
    ]
  }
}

# update backend2 server
resource "null_resource" "backend2_config" {
  depends_on = ["null_resource.backend1_config"]

  # provide some connection info
  connection {
    type = "ssh"
    user = "${module.backend2.ssh_username}"
    host = "${module.backend2.public_ipv4_dns}"
  }

  provisioner "file" {
    content     = "${data.template_file.hosts_config.rendered}"
    destination = "/tmp/hosts"
  }

  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "echo -e '\nBEGIN INSTALL CHEF BACKEND2\n'",
      "sudo chown root:root /tmp/hosts",
      "sudo mv /tmp/hosts /etc/hosts",
      "curl -vo /tmp/${replace(var.backend_version_url, "/^.*\\//", "")} ${var.backend_version_url}",
      "sudo ${replace(var.backend_version_url, "rpm", "") != var.backend_version_url ? "rpm -U" : "dpkg -iEG"} /tmp/${replace(var.backend_version_url, "/^.*\\//", "")}",
      "sudo chef-backend-ctl join-cluster --accept-license --yes --quiet ${var.enable_ipv6 == true ? module.backend1.public_ipv6_address : module.backend1.private_ipv4_address} -p ${var.enable_ipv6 == true ? module.backend2.public_ipv6_address : module.backend2.private_ipv4_address} -s /tmp/chef-backend-secrets.json",
      "echo -e '\nEND INSTALL CHEF BACKEND2\n'",
    ]
  }
}

# update backend2 server
resource "null_resource" "backend3_config" {
  depends_on = ["null_resource.backend2_config"]

  # provide some connection info
  connection {
    type = "ssh"
    user = "${module.backend3.ssh_username}"
    host = "${module.backend3.public_ipv4_dns}"
  }

  provisioner "file" {
    content     = "${data.template_file.hosts_config.rendered}"
    destination = "/tmp/hosts"
  }

  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "echo -e '\nBEGIN INSTALL CHEF BACKEND3\n'",
      "sudo chown root:root /tmp/hosts",
      "sudo mv /tmp/hosts /etc/hosts",
      "curl -vo /tmp/${replace(var.backend_version_url, "/^.*\\//", "")} ${var.backend_version_url}",
      "sudo ${replace(var.backend_version_url, "rpm", "") != var.backend_version_url ? "rpm -U" : "dpkg -iEG"} /tmp/${replace(var.backend_version_url, "/^.*\\//", "")}",
      "sudo chef-backend-ctl join-cluster --accept-license --yes --quiet ${var.enable_ipv6 == true ? module.backend1.public_ipv6_address : module.backend1.private_ipv4_address} -p ${var.enable_ipv6 == true ? module.backend3.public_ipv6_address : module.backend3.private_ipv4_address} -s /tmp/chef-backend-secrets.json",
      "echo -e '\nEND INSTALL CHEF BACKEND3\n'",
    ]
  }
}

# update chef server
resource "null_resource" "chef_server_config" {
  depends_on = ["null_resource.backend3_config"]

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
      "sudo mv /tmp/chef-server.rb /etc/opscode",
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
  depends_on = ["null_resource.chef_server_config"]

  connection {
    type = "ssh"
    user = "${module.chef_server.ssh_username}"
    host = "${module.chef_server.public_ipv4_dns}"
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

  # run psql test
  provisioner "remote-exec" {
    script = "${path.module}/../../../common/files/test_psql.sh"
  }

  # run gather-logs test
  provisioner "remote-exec" {
    script = "${path.module}/../../../common/files/test_gather_logs.sh"
  }
}
