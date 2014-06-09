#Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

add_command "upgrade", "Upgrade your private chef installation.", 1 do

  # Detect if OSC is present - if not, then skip to and continue with EC upgrade
  # Ask user if they want to upgrade
  if File.directory?("/opt/chef-server")
    puts "Open Source Chef server detected. Would you like to upgrade? [Yn]"

    # using gets fails with: No such file or directory - opscode
    #answer = gets.chomp
    #if answer == 'Y'
      puts "Upgrade from Open Source Chef"
    #else
    #  exit 0 "What do we want to do if the user says no?
    #end
  end

  # Start OSC (this assume EC isn't running , b/c if we detected OSC, then EC is assumed
  # not to be present)
  puts 'Ensuring the Open Source Chef server is started'
  status = run_command("chef-server-ctl start")
  if !status.success?
    puts "Unable to start Open Source Chef server, which is needed to complete the upgrade"
    exit 1
  end

  sleep(10) # start command can return faster than the services are ready; resulting in 502 gateway

  # knife download the data from the running OSC server
  # It downloads to whatever the current working dir is, so may need to switch to another dir
  # before running and then switch back
  # Or a directory to store the data can be specified in the config
  # Does it matter if the knife embedded with OSC or EC is used?

  puts 'Making /tmp/chef-server-data as a location to save the open source server data'
  # Should tighten up permissions
 data_dir = "/tmp/chef-server-data"
 Dir.mkdir(data_dir, 0777) unless File.directory?(data_dir)

  # Hardcoded path to key (stole idea to use from pedant), but the path is in attributes
  # Obviously a hard coded path to a server located at dev-vm isn't going to work in prod
  config = <<-EOH
  chef_server_url 'https://api.opscode.piab'
  node_name 'admin'
  client_key '/etc/chef-server/admin.pem'
  repo_mode 'everything'
  versioned_cookbooks true
  chef_repo_path '/tmp/chef-server-data'
  EOH

  puts "Writing knife config to /tmp/knife-config.rb for use in saving data"
  File.open("/tmp/knife-config.rb", "w"){ |file| file.write(config)}
  puts "Running knife download"
  status = run_command("/opt/chef-server/embedded/bin/knife download -c /tmp/knife-config.rb /")
  if !status.success?
    puts "knife download failed with #{status}; bailing"
    exit 1
  end

  puts 'Ensuring the Open Source Chef server is stopped'
  status = run_command("chef-server-ctl stop")
  if !status.success?
    puts "Unable to stop Open Source Chef server, which is needed to complete the upgrade"
    exit 1
  end

  # A note: bad things happen if the migration has to be run again after EC has been
  # configured on the box. Even if EC is stopped, conflicts will still occur around postgres
  # and the users on the system if it is attempted to run OSC again

  # OSC is down, time to bring up EC

  # In testing on a dev-vm, with OSC installed and running, then dpkg installed EC,
  # then loaded in opscode-omnibus to get this code w/ doing a full build, runninb
  # p-c-c upgrade without having first reconfigured EC caused the upgrade command to fail,
  # saying it couldn't find the enterprise cookbook
  # The reconfigure command was failing, due to missing cookbook dependencies. I assume this
  # was a quirk of the way I loaded in opscode-omnibus without it being on a vm already running EC.
  # After I dropped in the cookbooks by hand, all worked. Something to look out for later.
  reconfigure(false)

  # I think the reconfigure starts all the services, but let's be sure
  status = run_command("private-chef-ctl start")
  if !status.success?
    puts "Unable to start Enterprise Chef, which is needed to complete the upgrade"
    exit 1
  end
#
  sleep(10) # it takes a bit for the services to come up; sleep before hitting them with an org request 

  # Now we need an org
  puts "Creating org"
  require 'chef'

  chef_server_root = "https://api.opscode.piab" # hard coded to dev-vm, needs to point to EC
  # let's use the pivotal user, shall we?
  chef_rest = Chef::REST.new(chef_server_root, 'pivotal', '/etc/opscode/pivotal.pem')
  org_name = 'minitrue' # this is the org short name
  org_full_name = 'MinistryOfTruth'
  org_args = {:name => org_name , :full_name => org_full_name, :org_type => 'Business'}
  private_key = chef_rest.post('organizations/', org_args)

  # result of post will be the private key. Should probably stick that somewhere.
  File.open("/tmp/privatekey.pem", "w"){ |file| file.write(private_key)}

  # let's move around some data to start the transform from OSC to EC
  # manipulation of files as needed can come later

  puts "Transforming Data"

  # let's have a new top level dir
  new_data_dir = "/tmp/new-chef-server-data"
  Dir.mkdir(new_data_dir, 0777) unless File.directory?(new_data_dir)

  # put in place the org name structure
  Dir.mkdir("#{new_data_dir}/organizations") unless File.directory?("#{new_data_dir}/organizations")
  org_dir = "#{new_data_dir}/organizations/#{org_name}"
  Dir.mkdir(org_dir, 0777) unless File.directory?(org_dir)

  # users still live at the top level, so let's copy them over
  # data_dir is the original location the knife download data was saved
  FileUtils.cp_r("#{data_dir}/users", "#{new_data_dir}/users")

  # now we need to copy over clients, cookbooks, data_bags, environments, nodes, roles to their new home
  # under the organization structure
  %w{clients cookbooks data_bags environments nodes roles}.each do |name|
    FileUtils.cp_r("#{data_dir}/#{name}", "#{org_dir}/#{name}")
  end

  # what is missing at this point? At the top level next to organizations and users, user_acls is missing.
  # At the org level, next to the dirs of clients, et al. we're missing dirs for acls, containers, and groups

  # * user transform needed: add display_name, email; change name to user name
  # need to add org.json under organizations/#{org_name}
  # email is important, as it will be needed for password resets, since if we use knife ec backup
  # it doesn't move passwords
  #

  puts 'Transforming users'

  # * org.json needs: name, at the bare minimum (leaving the rest of it out, what will break?
  # doesn't cause an error if you only stick name in it)

  # * need invitations.json under organizations/#{org_name} (can be an empty array/object and work [])

  # * need members.json under organizations/#{org_name}. Should be an array/object with hashes for the
  # users in the org
  #
  # Members.json looks like this
  # (hmm, where do we specify the admins? Am I missing that and it goes here?):
  #
  # [
  #  {
  #   "user": { "username": "users's username"}
  #  },
  #  {
  #   "user": { "username": "users's username"}
  #  }
  # ]
  #
  # Under organizations/#{org_name}/groups, an admins.json and billing-admins.json is needed.
  # Will need to determine the users that go into both. admins should include the pivotal user.
  # pivotal does not need to go into billing-admins (does it matter who is in billing admins?
  # What happens to the admin user from OSC? (the base admin user, which is OSC's pivotal)
  #
  # Will need acl files - in a top level folder next to organizations/ and users/, called
  # user_acls/ and in it it has a #{username}.json file for each user to be imported
  #
  # For this POC, I copied a user_acl json file from a dev-vm user and replaced the user name.
  # Need to ensure the group is right for the read acl - it should be #{orgname}_global_admins
  # What effect will this have on the permissions of the system?
  # What would be sensible defaults to set here? Give everyone the world and then tell them to restrict it?
  #

  # access the data pulled from OSC
  users = []

  Dir.glob("#{data_dir}/users/*") do |file|
    user = Chef::JSONCompat.from_json(IO.read(file), :create_additions => false)
    users << user['name'] # user's names are needed several times later
    user['username'] = user['name']
    # the email address will need to be configurable in someway so that password resets work
    user['email'] = "#{user['username']}@example.com"
    user['display_name'] = user['username']
    user.delete('name')
    File.open("#{new_data_dir}/users/#{File.basename(file)}", "w"){ |new_file| new_file.write(Chef::JSONCompat.to_json_pretty(user)) }
  end

    org_json = { "name" => org_name }
    File.open("#{org_dir}/org.json", "w"){ |file| file.write(Chef::JSONCompat.to_json_pretty(org_json)) }

    File.open("#{org_dir}/invitations.json", "w"){ |file| file.write([])}

    members_json = []
    users.each do |name|
      user_json= {"user" => {"username" => name}}
      members_json << user_json
    end

    File.open("#{org_dir}/members.json", "w"){ |file| file.write(Chef::JSONCompat.to_json_pretty(members_json)) }


   Dir.mkdir("#{org_dir}/groups") unless File.directory?("#{org_dir}/groups")

   admin_users = users.clone
   admin_users << 'pivotal'
   admins_json = { "name" => "admins", "users" => admin_users }
   File.open("#{org_dir}/groups/admins.json", "w"){ |file| file.write(Chef::JSONCompat.to_json_pretty(admins_json)) }

  billing_admins_json = { "name" => "billing-admins", "users" => users}
  File.open("#{org_dir}/groups/billing-admins.json", "w"){ |file| file.write(Chef::JSONCompat.to_json_pretty(billing_admins_json)) }

  Dir.mkdir("#{new_data_dir}/user_acls") unless File.directory?("#{new_data_dir}/user_acls")
  users.each do |name|
    actors = ['pivotal', name]
    group1 = { "actors" => actors, "groups" => [] }
    group2 = { "actors" => actors, "groups" => ["#{org_name}_global_admins"] }
    acl_json = { "create" => group1, "read" => group2, "update" => group1, "delete" => group1, "grant" => group1 }
    File.open("#{new_data_dir}/user_acls/#{name}.json", "w"){ |file| file.write(Chef::JSONCompat.to_json_pretty(acl_json ))}
end

  # will need to use knife ec restore to push the data to the server (knife upload won't do the trick, since it is for OSC)
  # or else mimic what knife ec restore is doing (this is all part of the knife-ec-backup gem)

  # For the sake of speed, let's just install knife ec backup.
  # Probably can't rely on doing this for this actually install, but if need be we can shove it in omnibus or
  # rip out the guts and put them here
  puts "Installing knife ec backup"
  # probably need to check if this is installed first before trying to install
  result = run_command("/opt/opscode/embedded/bin/gem install --no-ri --no-rdoc knife-ec-backup")

  #knife ec backup contains knife ec restore, which is what we need

  #note that the default gem install on a dev-vm at this point appears to be the OSC embedded gem. Check if this flips over to EC
  #once OSC is removed

  puts result

  # Knife ec backup config, hard code values that maybe dev-vm specific
  config = <<-EOH
  chef_server_root 'https://api.opscode.piab'
  node_name 'pivotal'
  client_key '/etc/opscode/pivotal.pem'
  EOH

  puts "Writing knife ec backup config to /tmp/knife-ec-backup-config.rb"
  File.open("/tmp/knife-ec-backup-config.rb", "w"){ |file| file.write(config)}

  puts "Running data migration"
  ec_restore = "/opt/opscode/embedded/bin/knife ec restore -c /tmp/knife-ec-backup-config.rb #{new_data_dir}"
  migration_result = run_command(ec_restore)

  puts "The migration result is:"
  puts migration_result

  puts "It wasn't pretty, but Bob's your uncle. (https://en.wikipedia.org/wiki/Microsoft_Bob)"
  puts "Migration is complete"

  # Original EC add_command
  #reconfigure(false)
  #Dir.chdir(File.join(base_path, "embedded", "service", "partybus"))
  #bundle = File.join(base_path, "embedded", "bin", "bundle")
  #status = run_command("echo 'Sleeping for 2 minutes before migration' ; sleep 120 ; #{bundle} exec ./bin/partybus upgrade")
  #status = run_command("#{bundle} exec ./bin/partybus upgrade")
  #if status.success?
  #  puts "Chef Server Upgraded!"
  #  exit 0
  #else
  #  exit 1
  #end
end
