# Omnibus Standalone Upgrade From Stable

This directory contains the Terraform code used to instantiate a single Chef Infra Server utilizing Omnibus built artifacts as the install and upgrade packages.

The stable version of `chef-server` is installed and configured prior to an upgrade using the unstable artifact.

Once the upgrade has completed, the pedant tests are run against the server.
