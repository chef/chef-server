#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2011 Opscode, Inc.
#
# All Rights Reserved
#

runit_service "nagios" do
  action :disable
end

runit_service "fcgiwrap" do
  action :disable
end

runit_service "php-fpm" do
  action :disable
end

