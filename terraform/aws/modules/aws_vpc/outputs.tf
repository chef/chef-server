output "vpc_id" {
  value = "${aws_vpc.default.id}"
}

output "subnet_id" {
  value = "${aws_subnet.default.id}"
}

output "ipv4_cidr_block" {
  value = "${cidrsubnet(aws_vpc.default.cidr_block, 4, 1)}"
}

output "ipv6_cidr_block" {
  value = "${cidrsubnet(aws_vpc.default.ipv6_cidr_block, 8, 1)}"
}

output "region" {
  value = "${var.aws_region}"
}
