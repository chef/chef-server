# identify rhel 6 ami
data "aws_ami" "rhel_6" {
  most_recent = true

  filter {
    name   = "name"
    values = ["RHEL-6*"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }

  owners = ["309956199498"]
}

# identify rhel 7 ami
data "aws_ami" "rhel_7" {
  most_recent = true

  filter {
    name   = "name"
    values = ["RHEL-7*"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }

  owners = ["309956199498"]
}

# identify rhel 8 ami
data "aws_ami" "rhel_8" {
  most_recent = true

  filter {
    name   = "name"
    values = ["RHEL-8*"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }

  owners = ["309956199498"]
}

# identify ubuntu 16.04 ami
data "aws_ami" "ubuntu_1604" {
  most_recent = true

  filter {
    name   = "name"
    values = ["ubuntu/images/hvm-ssd/ubuntu-xenial-16.04-amd64-server-*"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }

  owners = ["099720109477"]
}

# identify ubuntu 18.04 ami
data "aws_ami" "ubuntu_1804" {
  most_recent = true

  filter {
    name   = "name"
    values = ["ubuntu/images/hvm-ssd/ubuntu-bionic-18.04-amd64-server-*"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }

  owners = ["099720109477"]
}

# identify suse 12 ami
data "aws_ami" "sles_12" {
  most_recent = true

  filter {
    name   = "name"
    values = ["suse-sles-12-sp*hvm-ssd-x86_64"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }

  owners = ["013907871322"]
}
