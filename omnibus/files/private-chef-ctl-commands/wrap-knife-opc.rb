#
# Copyright:: Copyright (c) 2014 Chef Software, Inc.
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

require 'shellwords'

# detect if running as a habitat service
if File.exist?('/hab/svc/chef-server-ctl/PID')
  knife_config = "/hab/svc/chef-server-ctl/config/pivotal.rb"
  knife_path = `cat /hab/svc/chef-server-ctl/config/pkg_path`.chomp + "/chef/bin/knife"
  bundler_path = `hab pkg path "core/bundler"`.chomp + "/bin/bundle"
  knife_cmd =  "#{bundler_path} exec #{knife_path}"
else
  knife_config = "/etc/opscode/pivotal.rb"
  knife_cmd    = "/opt/opscode/embedded/bin/knife"
end

cmd_args     = ARGV[3..-1]

cmds = {
  "org-create"      => ["create", "org", "Create an organization in the chef server."],
  "org-delete"      => ["delete", "org", "Delete an organization in the chef server."],
  "org-list"        => ["list", "org", "List all organizations in the chef server."],
  "org-show"        => ["show", "org", "Show an organization in the chef server."],
  "org-user-add"    => ["add", "org user", "Associate a user with an organization."],
  "org-user-remove" => ["remove", "org user", "Dissociate a user with an organization."],
  "user-create"     => ["create", "user", "Create a user in the chef server."],
  "user-delete"     => ["delete", "user", "Delete a user in the chef server."],
  "user-edit"       => ["edit", "user", "Edit a user in the chef server."],
  "user-list"       => ["list", "user", "List all users in the chef server."],
  "user-show"       => ["show", "user", "Show a user in the chef server."],
}

cmds.each do |cmd, args|
  opc_cmd = args[0]
  opc_noun = args[1]
  description = args[2]
  add_command_under_category cmd, "organization-and-user-management", description, 2 do
    escaped_args = cmd_args.map {|a| Shellwords.escape(a) }.join(' ')
    status = run_command("#{knife_cmd} opc #{opc_noun} #{opc_cmd} #{escaped_args} -c #{knife_config}")
    exit status.exitstatus
  end
end
