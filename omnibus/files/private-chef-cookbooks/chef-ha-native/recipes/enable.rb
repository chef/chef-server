#
# Author:: Steven Danna (<steve@chef.io>)
#
# Copyright 2015 Chef Software, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

if ENV["IM_A_CHEF_DEVELOPER_AND_KNOW_THE_CONSEQUENCES"].nil?
  Chef::Log.error <<EOF

You have tried to enable an unreleased feature of Chef Server.  Thank
you for your interest, but this feature is not yet ready for
prime-time!  In fact, it might not even be close to working!

If you are interested in testing this feature in development, please
set the environment variable IM_A_CHEF_DEVELOPER_AND_KNOW_THE_CONSEQUENCES.
EOF
  exit!(1)
else
  Chef::Log.warn <<EOF


You have enabled an unreleased feature of Chef Server. Please be aware
that this feature is not yet ready for release.  It might not even be
close to working!

By enabling this feature, you agree to the following oath:

On my honor, I will not use this feature in production. I swear to
fix the bugs I find and be suspicious of the ones I don't. I will
leave the code better than when I found it.
EOF
end

directory "#{node['private_chef']['keepalived']['dir']}/bin" do
  recursive true
end

# TODO We should name this file, but for now this lets us hook into an
# unmodified cluster.sh for testing
template "#{node['private_chef']['keepalived']['dir']}/bin/ha_backend_storage" do
  source "ha_backend_storage_native.erb"
  owner "root"
  group "root"
  mode "0755"
end

include_recipe "chef-ha-native::bookshelf-receiver"
