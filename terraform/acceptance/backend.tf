terraform {
  backend "s3" {
    profile = "chef-cd"
    bucket  = "chef-cd-terraform-state"
    region  = "us-west-2"
  }
}
