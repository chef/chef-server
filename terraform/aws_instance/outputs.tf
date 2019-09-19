output "public_ipv4_address" {
  value = "${aws_instance.default.public_ip}"
}

output "public_ipv4_dns" {
  value = "${aws_instance.default.public_dns}"
}

output "private_ipv4_address" {
  value = "${aws_instance.default.private_ip}"
}

output "private_ipv4_dns" {
  value = "${aws_instance.default.private_dns}"
}

output "public_ipv6_address" {
  value = "${element(aws_instance.default.ipv6_addresses, 0)}"
}

output "ssh_username" {
  value = "${replace(var.platform, "/ubuntu-.*/", "ubuntu") == "ubuntu" ? "ubuntu" : "ec2-user"}"
}
