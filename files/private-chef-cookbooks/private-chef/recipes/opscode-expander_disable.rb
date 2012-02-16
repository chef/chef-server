#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

runit_service "opscode-expander" do
  action :disable
end

runit_service "opscode-expander-reindexer" do
  action :disable
end




