variable "aws_profile" {
  type        = "string"
  description = "Name of the AWS profile used for authentication."
  default     = "chef-engineering"
}

variable "aws_region" {
  type        = "string"
  description = "Name of the AWS region to create instances in."
  default     = "us-west-1"
}

variable "aws_vpc_name" {
  type        = "string"
  description = "Name of the AWS virtual private cloud where tests will be run."
  default     = ""
}

variable "aws_department" {
  type        = "string"
  description = "Department that owns the resources should be one of: EngServ, Operations, Eng, Training, Solutions, Sales, BD, Success or Partner"
}

variable "aws_contact" {
  type        = "string"
  description = "The primary contact for the resources, this should be the IAM username and must be able to receive email by appending @chef.io to it (this person can explain what/why, might not be the business owner)."
}

variable "aws_ssh_key_id" {
  type        = "string"
  description = "AWS ID of the SSH key used to access the instance."
}

variable "aws_instance_type" {
  type        = "string"
  description = "Name of the AWS instance type used to determine size of instances."
  default     = "t3.medium"
}

variable "build_prefix" {
  type        = "string"
  description = "Optional build identifier for differentiating scenario runs."
  default     = ""
}

variable "enable_ipv6" {
  type        = "string"
  description = "Allocate an IPv6 address in addition to IPv4 address."
}

variable "platform" {
  type        = "string"
  description = "Operating System of the instance to be created."
}

variable "name" {
  type        = "string"
  description = "Name of the instance to be created."
}
