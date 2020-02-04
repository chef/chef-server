# Omnibus FIPS

This directory contains the Terraform code used to instantiate a single Chef Infra Server conforming to Federal Information Processing Standards (FIPS) utilizing an Omnibus built artifact downloaded from `$upgrade_version_url` as the install package.

Once the server has been installed and configured, the pedant tests are run against the server.

NOTE: This scenario can only be run against RHEL distributions.
