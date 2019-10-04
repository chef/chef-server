provider "aws" {
  profile = "${var.aws_profile}"
  region  = "${var.aws_region}"
}

resource "aws_vpc" "default" {
  enable_dns_support               = true
  enable_dns_hostnames             = true
  assign_generated_ipv6_cidr_block = true
  cidr_block                       = "10.0.0.0/16"

  tags = {
    Name      = "${local.vpc_name}"
    X-Dept    = "${var.aws_department}"
    X-Contact = "${var.aws_contact}"
  }
}

resource "aws_subnet" "default" {
  vpc_id = "${aws_vpc.default.id}"

  cidr_block              = "${cidrsubnet(aws_vpc.default.cidr_block, 4, 1)}"
  map_public_ip_on_launch = true

  ipv6_cidr_block                 = "${cidrsubnet(aws_vpc.default.ipv6_cidr_block, 8, 1)}"
  assign_ipv6_address_on_creation = true

  tags = {
    Name      = "${local.vpc_name}"
    X-Dept    = "${var.aws_department}"
    X-Contact = "${var.aws_contact}"
  }
}

resource "aws_internet_gateway" "default" {
  vpc_id = "${aws_vpc.default.id}"

  tags = {
    Name      = "${local.vpc_name}"
    X-Dept    = "${var.aws_department}"
    X-Contact = "${var.aws_contact}"
  }
}

resource "aws_default_route_table" "default" {
  default_route_table_id = "${aws_vpc.default.default_route_table_id}"

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = "${aws_internet_gateway.default.id}"
  }

  route {
    ipv6_cidr_block = "::/0"
    gateway_id      = "${aws_internet_gateway.default.id}"
  }

  tags = {
    Name      = "${local.vpc_name}"
    X-Dept    = "${var.aws_department}"
    X-Contact = "${var.aws_contact}"
  }
}

resource "aws_route_table_association" "default" {
  subnet_id      = "${aws_subnet.default.id}"
  route_table_id = "${aws_default_route_table.default.id}"
}
