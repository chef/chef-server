#Copyright:: Copyright (c) 2012-2014 Chef Software, Inc.
#
# All Rights Reserved
#

add_command "upgrade", "Upgrade your private chef installation.", 1 do

  # Detect if OSC is present - if not, then skip to and continue with EC upgrade
  # Ask user if they want to upgrade
  if File.directory?("/opt/chef-server")
    # Do we want to refer to it as the open source chef server, since the new
    # server is open source too, even if it is based on enterprise chef?
    puts "Open Source Chef server detected."

    #puts "Would you like to upgrade? [Yn]"
    # using gets fails with: No such file or directory - opscode
    #answer = gets.chomp
    #if answer == 'Y'
      puts "Upgrading the Open Source Chef server"
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

  puts "Preparing knife to download data from the Open Source Chef server"

  puts 'Making /tmp/chef-server-data as the location to save the server data'
  osc_data_dir = "/tmp/chef-server-data"
  # Are the permissions good enough?
  Dir.mkdir(osc_data_dir, 0644) unless File.directory?(osc_data_dir)

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

  puts "Writing knife config to /tmp/knife-config.rb for use in downloading the data"
  File.open("/tmp/knife-config.rb", "w"){ |file| file.write(config)}
  puts "Running knife download"
  status = run_command("/opt/chef-server/embedded/bin/knife download -c /tmp/knife-config.rb /")
  if !status.success?
    puts "knife download failed with #{status}"
    exit 1
  end
  puts "Finished downloading data from the Open Source Chef server"

  puts 'Ensuring the Open Source Chef server is stopped'
  status = run_command("chef-server-ctl stop")
  if !status.success?
    puts "Unable to stop Open Source Chef server, which is needed to complete the upgrade"
    exit 1
  end

  # Need to ensure all of OSC is stopped
  # By default, pkill sends TERM, which will cause runsv,runsvdir, and svlogd to shutdown
  # Doing this will completely hose OSC so that a start command won't restart it, if needed
  # If we're going to make this process as idempotent as possible, we'll need to work around this
  puts "Ensuring all the runit processes associated with the Open Source Server are stopped"
  run_command("pkill runsv")
  run_command("pkill runsvdir")
  run_command("pkill svlogd")
  # epmd lives outside of runit, but a pkill will do just fine here too
  run_command("pkill epmd")

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
  puts "Now configuring the Enterprise Chef server for use"
  reconfigure(false)

  # Let's ensure all the services are started
  puts "Ensuring all the Enterprise Chef server components are started"
  status = run_command("private-chef-ctl start")
  if !status.success?
    puts "Unable to start Enterprise Chef, which is needed to complete the upgrade"
    exit 1
  end
#
  sleep(30) # it takes a bit for the services to come up; sleep before hitting them with requests

  require 'chef'

  puts "Transforming Open Source server downloaded Data for upload to Enterprise Chef server"

  # let's have a new top level dir
  new_data_dir = "/tmp/new-chef-server-data"
  Dir.mkdir(new_data_dir, 0777) unless File.directory?(new_data_dir)

  puts "Creating a default Enterprise Chef organization to associate the data with"
  # we need a default org name
  # will need to either pick a sensible default, or somehow let the user specify this
  # or possibly even both
  org_name = 'minitrue'
  org_full_name = "MinistryOfTruth"
  org_type = "Business"

  # put in place the org name structure
  Dir.mkdir("#{new_data_dir}/organizations") unless File.directory?("#{new_data_dir}/organizations")
  org_dir = "#{new_data_dir}/organizations/#{org_name}"
  Dir.mkdir(org_dir, 0777) unless File.directory?(org_dir)

  # We need to fill out the info for the default org and place it in
  # org.json under organizations/#{org_name}
  # Not clear if/how the private key is retrieved in this case
  org_json = {"name" => org_name, "full_name" => org_full_name, "org_type" => org_type}
  File.open("#{org_dir}/org.json", "w"){ |file| file.write(Chef::JSONCompat.to_json_pretty(org_json)) }

  puts "Moving Open Source server data over to an Enterprise Chef server structure"

  # users still live at the top level, so let's copy them over
  FileUtils.cp_r("#{osc_data_dir}/users", "#{new_data_dir}/users")

  # now we need to copy over clients, cookbooks, data_bags, environments, nodes, roles to their new home
  # under the organization structure
  %w{clients cookbooks data_bags environments nodes roles}.each do |name|
    FileUtils.cp_r("#{osc_data_dir}/#{name}", "#{org_dir}/#{name}")
  end

  puts 'Transforming Open Source server user data to Enterprise Chef server format'

  # access the data pulled from OSC
  users = []

  # user transform needed: add display_name, email; change name to user name
  # email is important, as it will be needed for password resets, since if we use knife ec backup
  # it doesn't move passwords
  Dir.glob("#{osc_data_dir}/users/*") do |file|
    user = Chef::JSONCompat.from_json(IO.read(file), :create_additions => false)
    users << user['name'] # user's names are needed several times later
    user['username'] = user['name']
    # the email address will need to be configurable in someway so that password resets work
    user['email'] = "#{user['username']}@example.com"
    user['display_name'] = user['username']
    user.delete('name')
    File.open("#{new_data_dir}/users/#{File.basename(file)}", "w"){ |new_file| new_file.write(Chef::JSONCompat.to_json_pretty(user)) }
  end

  puts 'Creating an empty invitation list for the Enterprise Chef server'

  # need invitations.json under organizations/#{org_name} (can be an empty array/object and work [])
  File.open("#{org_dir}/invitations.json", "w"){ |file| file.write([])}

  puts 'Creating a members list for the Enterprise Chef server'

  # need members.json under organizations/#{org_name}. Should be an array/object with hashes for the
  # users in the org
  #
  # Members.json looks like this
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
  members_json = []
  users.each do |name|
    user_json= {"user" => {"username" => name}}
    members_json << user_json
  end

  File.open("#{org_dir}/members.json", "w"){ |file| file.write(Chef::JSONCompat.to_json_pretty(members_json)) }

  puts 'Creating groups for the Enterpise Chef server'

  # Under organizations/#{org_name}/groups, an admins.json and billing-admins.json is needed.
  # Will need to determine the users that go into both. admins should include the pivotal user.
  # pivotal does not need to go into billing-admins (does it matter who is in billing admins?
  # Any admins from OSC need to go into the admins group, as that is how it is determined that
  # a user is an admin in EC
  Dir.mkdir("#{org_dir}/groups") unless File.directory?("#{org_dir}/groups")

  puts 'Creating admins group'

  admin_users = users.clone
  admin_users << 'pivotal'
  admins_json = { "name" => "admins", "users" => admin_users }
  File.open("#{org_dir}/groups/admins.json", "w"){ |file| file.write(Chef::JSONCompat.to_json_pretty(admins_json)) }

  puts 'Creating billing_admins group'

  billing_admins_json = { "name" => "billing-admins", "users" => users}
  File.open("#{org_dir}/groups/billing-admins.json", "w"){ |file| file.write(Chef::JSONCompat.to_json_pretty(billing_admins_json)) }

  # Use knife ec backup to put the data into the Enterprise Chef server
  # (Specificly knife ec restore, which is in the knife ec backup gem)
  # This should be included in the opscode-omnibus install, not downloaded from net
  # as it is in this POC
  # For now, download the beta release, to get all the latest goodness
  puts "Installing knife ec backup"
  # knife ec backup uses the pg gem. The --with-pg-config option gets passed through
  # to the pg gem here so it can use the option when building a native extension,
  # this way it can talk to the Chef db. This enables knife-ec-backup to pull across
  # db items, such as passwords
  result = run_command("/opt/opscode/embedded/bin/gem install --pre --no-ri --no-rdoc knife-ec-backup -- --with-pg-config=/opt/opscode/embedded/postgresql/9.2/bin/pg_config")
  puts result

  # Knife ec backup config, hard code values that maybe dev-vm specific
  config = <<-EOH
  chef_server_root 'https://api.opscode.piab'
  node_name 'pivotal'
  client_key '/etc/opscode/pivotal.pem'
  EOH

  puts "Writing knife ec backup config to /tmp/knife-ec-backup-config.rb"
  File.open("/tmp/knife-ec-backup-config.rb", "w"){ |file| file.write(config)}

  puts "Uploading transformed Open Source server data to Enterprise Chef server"
  puts "Running data migration"

  # --skip-useracl skip importing user acls, which will just give the user's default acls. This is the
  # desired state anway
  # --with-user-sql pull data across from the database, so we can get passwords
  # --concurrency 1 so that it doesn't try concurrent cookbook uploads; there appears to be a bug
  # around concurrent uploads
  ec_restore = "/opt/opscode/embedded/bin/knife ec restore --skip-useracl --with-user-sql --concurrency 1 -c /tmp/knife-ec-backup-config.rb #{new_data_dir}"
  migration_result = run_command(ec_restore)

  # Need to capture better output/bail if this isn't successful
  puts "The migration result is:"
  puts migration_result
  puts "It wasn't pretty, but Bob's your uncle. (https://en.wikipedia.org/wiki/Microsoft_Bob)"
  puts "Migration is complete"
  puts "Open Source chef server upgraded to an Enterprise Chef server"

  # The OSC bits still live on the system - do we delete them here?
  # For example, /opt/chef-server is still in the path, but /opt/opscode is not
  # on dev-vm testing
  # This has the effect of making the default knife, gem, etc the chef-server versions


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
