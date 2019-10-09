# identify external ipv4 address of the system where terraform is being run from
data "http" "workstation-ipv4" {
  url = "http://ipv4.icanhazip.com"
}

locals {
  workstation-ipv4-cidr = "${chomp(data.http.workstation-ipv4.body)}/32"

  vpc_name = "${var.aws_vpc_name != "" ? var.aws_vpc_name : "${var.aws_contact}-chef_server-test"}"

  ami_ids = {
    rhel-6       = "${data.aws_ami.rhel_6.id}"
    rhel-7       = "${data.aws_ami.rhel_7.id}"
    rhel-8       = "${data.aws_ami.rhel_8.id}"
    ubuntu-16.04 = "${data.aws_ami.ubuntu_1604.id}"
    ubuntu-18.04 = "${data.aws_ami.ubuntu_1804.id}"
    sles-12      = "${data.aws_ami.sles_12.id}"
  }
}
