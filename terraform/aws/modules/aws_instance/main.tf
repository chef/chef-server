data "aws_vpc" "chef_vpc" {
  tags = {
    Name      = local.vpc_name
    X-Dept    = var.aws_department
    X-Contact = var.aws_contact
  }
}

data "aws_subnet" "chef_subnet" {
  tags = {
    Name      = local.vpc_name
    X-Dept    = var.aws_department
    X-Contact = var.aws_contact
  }
}

resource "aws_security_group" "default" {
  name_prefix = local.vpc_name
  vpc_id      = data.aws_vpc.chef_vpc.id

  ingress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = [cidrsubnet(data.aws_vpc.chef_vpc.cidr_block, 4, 1), local.workstation-ipv4-cidr]
  }

  ingress {
    from_port        = 0
    to_port          = 0
    protocol         = "-1"
    ipv6_cidr_blocks = [cidrsubnet(data.aws_vpc.chef_vpc.ipv6_cidr_block, 8, 1)]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port        = 0
    to_port          = 0
    protocol         = "-1"
    ipv6_cidr_blocks = ["::/0"]
  }

  tags = {
    Name      = "${var.build_prefix}${var.name}-${local.vpc_name}"
    X-Dept    = var.aws_department
    X-Contact = var.aws_contact
  }
}

resource "aws_instance" "default" {
  ami           = local.ami_ids[var.platform]
  instance_type = var.aws_instance_type == "t3.medium" && var.platform == "rhel-6" ? "t2.medium" : var.aws_instance_type
  key_name      = var.aws_ssh_key_id

  root_block_device {
    volume_size = 50
  }

  ipv6_address_count = var.enable_ipv6 == "true" ? 1 : 0

  subnet_id = data.aws_subnet.chef_subnet.id

  vpc_security_group_ids = [
    aws_security_group.default.id,
  ]

  tags = {
    Name      = "${var.build_prefix}${var.name}-${local.vpc_name}"
    X-Dept    = var.aws_department
    X-Contact = var.aws_contact
  }
}
