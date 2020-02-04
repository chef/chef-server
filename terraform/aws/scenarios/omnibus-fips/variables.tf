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
  description = "Name of the AWS instance type used to determine size of instances (e.g. t2.medium)."
  default     = "t2.medium"
}

variable "platform" {
  type        = "string"
  description = "Operating System of the instance to be created."
}

variable "build_prefix" {
  type        = "string"
  description = "Optional build identifier for differentiating scenario runs."
  default     = ""
}

#########################################################################
# Chef Server
#########################################################################
variable "scenario" {
  type        = "string"
  description = "The name of the scenario being executed."
}

variable "install_version_url" {
  type        = "string"
  description = "The URL to a chef-server used during initial install."
}

variable "upgrade_version_url" {
  type        = "string"
  description = "The URL to a chef-server artifact used during upgrades."
}

variable "enable_ipv6" {
  type        = "string"
  description = "Use IPv6 in the chef-server.rb config and /etc/hosts."
}
