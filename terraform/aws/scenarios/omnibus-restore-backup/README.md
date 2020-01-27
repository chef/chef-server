# Omnibus Standalone Fresh Install

This directory contains the Terraform code used to instantiate a single Chef Infra Server utilizing an Omnibus built artifact downloaded from `$upgrade_version_url` as the install package.

Once the server has been installed and configured, the specified backup is copied from the workstation to the server
and restored.
