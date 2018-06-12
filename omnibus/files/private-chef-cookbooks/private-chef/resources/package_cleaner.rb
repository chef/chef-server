# Copyright:: 2013-2018 Chef Software, Inc.
# All Rights Reserved

# Perform safe cleanup after old packages and services following a
# successful Private Chef upgrade.  Executables will have been removed
# from the package upgrade process, but old data, configuration, logs,
# directories, etc. can be left behind.

# Completely clean up after a given service / package
actions :clean

default_action :clean

attribute :package,
:kind_of => String,
:name_attribute => true

# Directories that should be recursively removed
attribute :directories,
:kind_of => Array,
:default => []

# Files that should be removed
attribute :files,
:kind_of => Array,
:default => []

# Links that should be unlinked
attribute :links,
:kind_of => Array,
:default => []

# Users created for this service that should be removed
attribute :users,
:kind_of => Array,
:default => []

# Groups created for this service that should be removed
attribute :groups,
:kind_of => Array,
:default => []

# Some packages to be removed are also governed by runit services.  In
# that case, we'll need to remove the control files, links, etc, for
# those services
attribute :is_service,
:kind_of => [TrueClass, FalseClass],
:default => true

# Directory where runit service folders are dropped off.
attribute :service_root,
:kind_of => String,
:default => "/opt/opscode/sv"

# Directory where *links* to the runit service folders are dropped off
attribute :service_link_root,
:kind_of => String,
:default => "/opt/opscode/service"

# Directory where links to the sv binary are dropped off
attribute :service_init_link_root,
:kind_of => String,
:default => "/opt/opscode/init"
