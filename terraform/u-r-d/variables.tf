#########################################################################
# Default
#########################################################################
# Environment and the associated delivery environment
# Can be one of dev, acceptance, union, rehearsal or delivered
variable "environment" {
  type = "string"
}

#########################################################################
# Automate Pipeline
#########################################################################
variable "automate_project" {
  description = "Name of the Automate Workflow project that manages this code"
}

variable "automate_pipeline" {
  description = "Name of the Automate Workflow pipeline that manages this code"
}

#########################################################################
# AWS
#########################################################################
variable "aws_key_name" {
  description = "AWS keypair name to provison instances with"
}

variable "aws_private_key" {
  description = "File path to private key to provision with"
}

#########################################################################
# Chef
#########################################################################
variable "chef_environment" {
  description = "Chef environments new nodes will join"
}

variable "chef_server_url" {
  description = "Chef Server to bootstrap nodes against"
}

variable "chef_user_name" {
  description = "The name of the Chef user to register the new Chef Client with"
}

variable "chef_user_key" {
  description = "File path to the user key to authenticate with the Chef Server"
}
