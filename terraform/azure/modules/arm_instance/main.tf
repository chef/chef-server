provider "azurerm" {
  subscription_id = "${var.arm_subscription_id}"
  tenant_id       = "${var.arm_tenant_id}"
}

data "azurerm_resource_group" "chef_resource_group" {
  name = "${local.arm_resource_group_name}"
}

data "azurerm_virtual_network" "chef_virtual_network" {
  resource_group_name = "${data.azurerm_resource_group.chef_resource_group.name}"
  name                = "${local.arm_resource_group_name}"
}

data "azurerm_subnet" "chef_subnet" {
  resource_group_name  = "${data.azurerm_resource_group.chef_resource_group.name}"
  virtual_network_name = "${data.azurerm_virtual_network.chef_virtual_network.name}"

  name = "${local.arm_resource_group_name}"
}

resource "azurerm_network_security_group" "default" {
  resource_group_name = "${data.azurerm_resource_group.chef_resource_group.name}"
  location            = "${data.azurerm_resource_group.chef_resource_group.location}"

  name = "${var.build_prefix}${var.name}-${local.arm_resource_group_name}"

  security_rule {
    name                       = "All_From_${var.arm_contact}"
    priority                   = 1001
    direction                  = "Inbound"
    access                     = "Allow"
    protocol                   = "*"
    source_port_range          = "*"
    destination_port_range     = "*"
    source_address_prefix      = "${local.workstation-ipv4-cidr}"
    destination_address_prefix = "*"
  }

  tags = {
    X-Dept    = "${var.arm_department}"
    X-Contact = "${var.arm_contact}"
  }
}

resource "azurerm_public_ip" "default" {
  depends_on = ["azurerm_network_security_group.default"]

  resource_group_name = "${data.azurerm_resource_group.chef_resource_group.name}"
  location            = "${data.azurerm_resource_group.chef_resource_group.location}"

  name = "${var.build_prefix}${var.name}-${local.arm_resource_group_name}"

  allocation_method = "Dynamic"

  tags = {
    X-Dept    = "${var.arm_department}"
    X-Contact = "${var.arm_contact}"
  }
}

resource "azurerm_network_interface" "default" {
  depends_on = ["azurerm_public_ip.default"]

  resource_group_name       = "${data.azurerm_resource_group.chef_resource_group.name}"
  location                  = "${data.azurerm_resource_group.chef_resource_group.location}"
  network_security_group_id = "${azurerm_network_security_group.default.id}"

  name = "${var.build_prefix}${var.name}-${local.arm_resource_group_name}"

  ip_configuration {
    name                          = "${var.build_prefix}${var.name}-${local.arm_resource_group_name}"
    subnet_id                     = "${data.azurerm_subnet.chef_subnet.id}"
    private_ip_address_allocation = "Dynamic"
    public_ip_address_id          = "${azurerm_public_ip.default.id}"
  }

  tags = {
    X-Dept    = "${var.arm_department}"
    X-Contact = "${var.arm_contact}"
  }
}

resource "azurerm_virtual_machine" "default" {
  depends_on = ["azurerm_network_interface.default"]

  resource_group_name = "${data.azurerm_resource_group.chef_resource_group.name}"
  location            = "${data.azurerm_resource_group.chef_resource_group.location}"

  name                          = "${var.build_prefix}${var.name}-${local.arm_resource_group_name}"
  vm_size                       = "${var.arm_instance_type}"
  network_interface_ids         = ["${azurerm_network_interface.default.id}"]
  delete_os_disk_on_termination = true

  storage_os_disk {
    name              = "${var.build_prefix}${var.name}-${local.arm_resource_group_name}"
    caching           = "ReadWrite"
    create_option     = "FromImage"
    managed_disk_type = "Standard_LRS"
  }

  storage_image_reference {
    publisher = "${lookup(local.storage_images[var.platform], "publisher")}"
    offer     = "${lookup(local.storage_images[var.platform], "offer")}"
    sku       = "${lookup(local.storage_images[var.platform], "sku")}"
    version   = "${lookup(local.storage_images[var.platform], "version")}"
  }

  os_profile {
    computer_name  = "${var.build_prefix}${var.name}"
    admin_username = "azure"
  }

  os_profile_linux_config {
    disable_password_authentication = true

    ssh_keys {
      path     = "/home/azure/.ssh/authorized_keys"
      key_data = "${file(var.arm_ssh_key_file)}"
    }
  }

  tags = {
    X-Dept    = "${var.arm_department}"
    X-Contact = "${var.arm_contact}"
  }
}
