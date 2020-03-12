terraform {
  required_version = ">= 0.12.23"
}

provider "aws" {
  version = "~> 2.53"

  profile = var.aws_profile
  region  = var.aws_region
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
