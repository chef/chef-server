#
# Copyright:: Copyright (c) 2014 Chef Software, Inc.
#
# All Rights Reserved
#

knife_config = "/tmp/pivotal.rb"
knife_cmd    = "/opt/opscode/embedded/bin/knife"
cmd_args     = ARGV[3..-1]

def file_open(file, mode, &block)
  begin
    File.open(file, mode, &block)
  rescue Exception => e
    log "Received exception #{e.message} when trying to open file #{file}"
    exit 1
  end
end

config = <<-EOH
node_name "pivotal"
chef_server_url "https://localhost"
chef_server_root "https://localhost"
client_key "/etc/opscode/pivotal.pem"
EOH

file_open(knife_config, "w"){ |file| file.write(config)}

cmds = {
  "org-create"  => ["create", "org", "Create an organization in the chef server."],
  "org-delete"  => ["delete", "org", "Delete an organization in the chef server."],
  "org-list"    => ["list", "org", "List all organizationsin the chef server."],
  "org-show"    => ["show", "org", "Show an organization in the chef server."],
  "user-create" => ["create", "user", "Create a user in the chef server."],
  "user-delete" => ["delete", "user", "Delete a user in the chef server."],
  "user-edit"   => ["edit", "user", "Edit a user in the chef server."],
  "user-list"   => ["list", "user", "List all users in the chef server."],
  "user-show"   => ["show", "user", "Show a user in the chef server."],
}

cmds.each do |cmd, args|
  opc_cmd = args[0]
  opc_noun = args[1]
  description = args[2]
  add_command cmd, description, 2 do
    run_command("#{knife_cmd} opc #{opc_noun} #{opc_cmd} #{cmd_args.join(' ')} -c #{knife_config}")
  end
end
