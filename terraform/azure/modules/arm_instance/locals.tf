# identify external ipv4 address of the system where terraform is being run from
data "http" "workstation-ipv4" {
  url = "http://ipv4.icanhazip.com"
}

locals {
  workstation-ipv4-cidr = "${chomp(data.http.workstation-ipv4.body)}/32"

  arm_resource_group_name = "${var.arm_resource_group_name != "" ? var.arm_resource_group_name : "${var.arm_contact}-chef_server-test"}"

  storage_images = {
    rhel-6 = {
      publisher = "RedHat"
      offer     = "RHEL"
      sku       = "6.9"
      version   = "latest"
    }

    rhel-7 = {
      publisher = "RedHat"
      offer     = "RHEL"
      sku       = "7.7"
      version   = "latest"
    }

    rhel-8 = {
      publisher = "RedHat"
      offer     = "RHEL"
      sku       = "8"
      version   = "latest"
    }

    ubuntu-16.04 = {
      publisher = "Canonical"
      offer     = "UbuntuServer"
      sku       = "16.04-LTS"
      version   = "latest"
    }

    ubuntu-18.04 = {
      publisher = "Canonical"
      offer     = "UbuntuServer"
      sku       = "18.04-LTS"
      version   = "latest"
    }

    sles-12 = {
      publisher = "SUSE"
      offer     = "SLES"
      sku       = "12.SP4"
      version   = "latest"
    }
  }
}
