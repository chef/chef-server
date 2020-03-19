locals {
  vpc_name = var.aws_vpc_name != "" ? var.aws_vpc_name : "${var.aws_contact}-chef_server-test"
}
