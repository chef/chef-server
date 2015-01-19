#
# Copyright:: Copyright (c) 2014 Chef Software, Inc.
#
# All Rights Reserved
#

knife_config = "/etc/opscode/pivotal.rb"
knife_cmd    = "/opt/opscode/embedded/bin/knife"
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
    status = run_command("#{knife_cmd} opc #{opc_noun} #{opc_cmd} #{cmd_args.join(' ')} -c #{knife_config}")
    exit status.exitstatus
  end
end
