#Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

add_command "upgrade", "Upgrade your private chef installation.", 1 do

  # Detect if OSC is present
  # Ask user if they want to upgrade (and then ignore them and just upgrade in this POC)
  if File.directory?("/opt/chef-server")
    puts "Open Source Chef server detected. Would you like to upgrade? [Yn]"
    
    # using gets fails with: No such file or directory - opscode

    #answer = gets.chomp
    #if answer == 'Y'
      puts "Upgrade from Open Source Chef"
    #else
    #  puts "Not upgrading"
      # Just bail out for now
    #  exit 0
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
 org_short_name = 'minitrue'
  org_full_name = 'MinistryOfTruth'
  org_args = {:name => org_short_name , :full_name => org_full_name, :org_type => 'Business'}
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
  org_dir = "#{new_data_dir}/organizations/#{org_short_name}"
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

  # will need to use knife ec restore to push the data to the server (knife upload won't do the trick, since it is for OSC)
  # or else mimic what knife ec restore is doing (this is all part of the knife-ec-backup gem)

  # For the sake of speed, let's just install knife ec backup.
  # Probably can't rely on doing this for this actually install, but if need be we can shove it in omnibus or
  # rip out the guts and put them here
  puts "Installing knife ec backup"
  # probably need to check if this is installed first before trying to install
  result = run_command("/opt/opscode/embedded/bin/gem install --no-ri --no-rdoc knife-ec-backup")

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
