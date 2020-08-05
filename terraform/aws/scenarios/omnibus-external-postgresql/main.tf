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
  name              = "chef_server-${var.scenario}-${var.enable_ipv6 == "true" ? "ipv6" : "ipv4"}-${var.platform}"
}

module "postgresql" {
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
  name              = "postgresql-${var.scenario}-${var.enable_ipv6 == "true" ? "ipv6" : "ipv4"}-${var.platform}"
}

# generate static hosts configuration
data "template_file" "hosts_config" {
  template = file("${path.module}/templates/hosts.tpl")

  vars = {
    chef_server_ip = var.enable_ipv6 == "true" ? module.chef_server.public_ipv6_address : module.chef_server.private_ipv4_address
    postgresql_ip  = var.enable_ipv6 == "true" ? module.postgresql.public_ipv6_address : module.postgresql.private_ipv4_address
  }
}

# generate chef_server.rb configuration
data "template_file" "chef_server_rb" {
  template = file("${path.module}/templates/chef-server.rb.tpl")

  vars = {
    enable_ipv6   = var.enable_ipv6
    postgresql_ip = var.enable_ipv6 == "true" ? module.postgresql.public_ipv6_address : module.postgresql.private_ipv4_address
  }
}

# update postgres server
resource "null_resource" "postgresql_config" {
  # provide some connection info
  connection {
    type = "ssh"
    user = module.postgresql.ssh_username
    host = module.postgresql.public_ipv4_dns
  }

  provisioner "file" {
    content     = data.template_file.hosts_config.rendered
    destination = "/tmp/hosts"
  }

  # upload private.key if running from buildkite
  provisioner "local-exec" {
    command = "[ -n \"$BUILDKITE\" ] && { vault read -field=private.key secret/chef-server | ssh -o 'UserKnownHostsFile=/dev/null' -o 'StrictHostKeyChecking=no' ${module.postgresql.ssh_username}@${module.postgresql.public_ipv4_dns} 'cat > /tmp/private.key'; } || true"
  }

  # upload certificate.pem if running from buildkite
  provisioner "local-exec" {
    command = "[ -n \"$BUILDKITE\" ] && { vault read -field=certificate.pem secret/chef-server | ssh -o 'UserKnownHostsFile=/dev/null' -o 'StrictHostKeyChecking=no' ${module.postgresql.ssh_username}@${module.postgresql.public_ipv4_dns} 'cat > /tmp/certificate.pem'; } || true"
  }

  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "echo -e '\nBEGIN INSTALL POSTGRESQL SERVER\n'",
      "sudo chown root:root /tmp/hosts",
      "sudo mv /tmp/hosts /etc/hosts",
      "sleep 30",
      "echo 'deb http://apt.postgresql.org/pub/repos/apt/ xenial-pgdg main' | sudo tee /etc/apt/sources.list.d/pgdg.list",
      "wget --quiet https://www.postgresql.org/media/keys/ACCC4CF8.asc",
      "sudo apt-key add ACCC4CF8.asc",
      "sudo apt-get update",
      "sudo apt-get install -y ssl-cert sysstat postgresql-9.6",
      "[ -f /tmp/private.key ] && cat /tmp/private.key | sudo tee /etc/ssl/private/ssl-cert-snakeoil.key >/dev/null",
      "[ -f /tmp/certificate.pem ] && cat /tmp/certificate.pem | sudo tee /etc/ssl/certs/ssl-cert-snakeoil.pem >/dev/null",
      "echo 'host    all             all            ${var.enable_ipv6 == "true" ? module.chef_server.public_ipv6_address : module.chef_server.private_ipv4_address}/${var.enable_ipv6 == "true" ? 64 : 32}         md5' | sudo tee -a /etc/postgresql/9.6/main/pg_hba.conf",
      "echo \"listen_addresses='*'\" | sudo tee -a /etc/postgresql/9.6/main/postgresql.conf",
      "sudo systemctl restart postgresql",
      "sudo -u postgres psql -c \"CREATE USER bofh SUPERUSER ENCRYPTED PASSWORD 'i1uvd3v0ps';\"",
      "echo -e '\nEND INSTALL POSTGRESQL SERVER\n'",
    ]
  }
}

# update chef server
resource "null_resource" "chef_server_config" {
  depends_on = [null_resource.postgresql_config]

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
    content     = data.template_file.chef_server_rb.rendered
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
      "echo -e '\nEND INSTALL CHEF SERVER\n'",
    ]
  }

  # add user + organization
  provisioner "remote-exec" {
    script = "${path.module}/../../../common/files/add_user.sh"
  }

  # run smoke test
  provisioner "remote-exec" {
    script = "${path.module}/../../../common/files/test_chef_server-smoke.sh"
  }
}

# enable ssl enforcement postgres server
resource "null_resource" "postgresql_config_ssl" {
  depends_on = [null_resource.chef_server_config]

  # provide some connection info
  connection {
    type = "ssh"
    user = module.postgresql.ssh_username
    host = module.postgresql.public_ipv4_dns
  }

  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "echo -e '\nBEGIN ENABLE SSL MODE ON POSTGRESQL SERVER\n'",
      "sudo sed -i '$ s/host/hostssl/' /etc/postgresql/9.6/main/pg_hba.conf",
      "sudo systemctl restart postgresql",
      "echo -e '\nEND ENABLE SSL MODE ON POSTGRESQL SERVER\n'",
    ]
  }
}

resource "null_resource" "chef_server_config_ssl" {
  depends_on = [null_resource.postgresql_config_ssl]

  connection {
    type = "ssh"
    user = module.chef_server.ssh_username
    host = module.chef_server.public_ipv4_dns
  }

  # enable chef-server ssl enforcement
  provisioner "remote-exec" {
    inline = [
      "set -evx",
      "echo -e '\nBEGIN ENABLE SSL MODE ON CHEF SERVER\n'",
      "sudo sed -i '/sslmode/{s/disable/require/;}' /etc/opscode/chef-server.rb",
      "sudo chef-server-ctl reconfigure --chef-license=accept",
      "sleep 120",
      "echo -e '\nEND ENABLE SSL MODE ON CHEF SERVER\n'",
    ]
  }
}

resource "null_resource" "chef_server_test" {
  depends_on = [null_resource.chef_server_config_ssl]

  connection {
    type = "ssh"
    user = module.chef_server.ssh_username
    host = module.chef_server.public_ipv4_dns
  }

  # upload test scripts
  provisioner "file" {
    source      = "${path.module}/../../../common/files/test_chef_server-smoke.sh"
    destination = "/tmp/test_chef_server-smoke.sh"
  }

  provisioner "file" {
    source      = "${path.module}/../../../common/files/install_addon_push_jobs.sh"
    destination = "/tmp/install_addon_push_jobs.sh"
  }

  provisioner "file" {
    source      = "${path.module}/../../../common/files/test_addon_push_jobs.sh"
    destination = "/tmp/test_addon_push_jobs.sh"
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

  # install + test push jobs addon
  provisioner "remote-exec" {
    inline = [
      "chmod +x /tmp/install_addon_push_jobs.sh",
      "ENABLE_ADDON_PUSH_JOBS=${var.enable_addon_push_jobs} /tmp/install_addon_push_jobs.sh",
      "chmod +x /tmp/test_addon_push_jobs.sh",
      "ENABLE_ADDON_PUSH_JOBS=${var.enable_addon_push_jobs} /tmp/test_addon_push_jobs.sh",
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
