#
# Copyright:: Copyright (c) 2012 Opscode, Inc.
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
  chef_server_url 'http://api.opscode.piab'
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

  # Now we need an org
  



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
