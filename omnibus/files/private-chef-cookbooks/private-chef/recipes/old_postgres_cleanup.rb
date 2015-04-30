#
# Author:: Christopher Maier (<cm@opscode.com>)
# Copyright:: Copyright (c) 2013 Opscode, Inc.
#
# All Rights Reserved

# Prior to Private Chef 11, the PostgreSQL service was named
# 'postgres', not 'postgresql'.  We need to cleanup all traces of the
# previously-named service if it is present.

runit_service "postgres" do
  action [:stop, :disable]
end

# I wish runit_service had a :destroy action...
directory "/opt/opscode/sv/postgres" do
  action :delete
  recursive true
end
