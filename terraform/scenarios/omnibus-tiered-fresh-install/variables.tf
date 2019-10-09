#########################################################################
# AWS
#########################################################################
variable "aws_profile" {
  type        = "string"
  description = "Name of the AWS profile used for authentication (e.g. chef-engineering)."
  default     = "chef-engineering"
}

variable "aws_region" {
  type        = "string"
  description = "Name of the AWS region to create instances in (e.g. us-west-2)."
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
  description = "AWS ID of the SSH key used to access the instance (e.g. csnapp)."
}

variable "aws_instance_type" {
  type        = "string"
  description = "Name of the AWS instance type used to determine size of instances (e.g. t3.medium)."
  default     = "t3.medium"
}

variable "platform" {
  type        = "string"
  description = "Operating System of the instance to be created."
  default     = "ubuntu-16.04"
}

#########################################################################
# Chef Server
#########################################################################
variable "unstable_version" {
  type        = "string"
  description = "The version of chef server build artifact to install (e.g. 13.0.38+20190904060033)"
}

variable "enable_ipv6" {
  type        = "string"
  description = "Use IPv6 in the chef-server.rb config and /etc/hosts."
  default     = "true"
}
