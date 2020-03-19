terraform {
  required_version = ">= 0.12.23"
}

provider "azurerm" {
  version = "~> 2.1"
  features {}

  subscription_id = var.arm_subscription_id
  tenant_id       = var.arm_tenant_id
}

provider "http" {
	version = "~> 1.1"
}

provider "null" {
	version = "~> 2.1"
}

provider "template" {
	version = "~> 2.1"
}
