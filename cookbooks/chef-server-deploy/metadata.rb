name 'chef-server-deploy'
maintainer 'Chef Software Inc Engineering'
maintainer_email 'engineering@chef.io'
license 'Apache 2.0'
description 'Installs/Configures Chef Server in ACC'
long_description 'Installs/Configures Chef Server in ACC'
version '0.1.2'

chef_version '>= 12.1' if respond_to?(:chef_version)

depends 'cd-deploy'
depends 'chefops-dcc'
