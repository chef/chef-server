# Omnibus Standalone Upgrade

This directory contains the Terraform code used to instantiate a single Chef Infra Server utilizing Omnibus built artifacts as the install and upgrade packages.

The `chef-server` artifact is downloaded from `$install_version_url`, installed, and configured prior to an upgrade using the artifact downloaded from `$upgrade_version_url`.

Once the upgrade has completed, the pedant tests are run against the server.
