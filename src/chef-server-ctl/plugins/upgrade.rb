# Copyright:: Copyright (c) 2012-2014 Chef Software, Inc.
#
# All Rights Reserved
#

require "optparse"
require "ostruct"

add_command_under_category "upgrade", "general", "Upgrade your Chef Infra Server installation after updating packages.", 2 do

  # Since this is evaled, need to have methods first so they can be picked up

  def parse(args)
    @options = OpenStruct.new

    # Define defaults
    @options.skip_confirmation = false
    @options.chef11_server_url = "https://localhost"
    @options.chef12_server_url = "https://localhost"
    @options.upload_threads = 10
    @options.chef11_admin_client_name = "admin"
    @options.chef11_admin_client_key = "/etc/chef-server/admin.pem"

    opt_parser = OptionParser.new do |opts|
      opts.banner = "Usage: chef-server-ctl upgrade [options]"
      opts.banner = opts.banner << "\n Options only apply to open source Chef 11 server to Chef 12 server upgrades."
      opts.banner = opts.banner << "\n If upgrading from Enterprise Chef 11 server to Chef 12 server no options are needed."

      opts.on("-y", "--yes", "Skip confirmation") do |y|
        @options.skip_confirmation = y
      end

      opts.on("-o", "--org-name [name]", String, "The name of the Chef 12 organization to be created. It must begin with a lower case letter or digit; can only have lower case letters, digits, hyphens, and underscores and must be between 1 and 255 characters long (Will ask interactively if not passed).") do |n|
        @options.org_name = n
      end

      opts.on("-f", "--full-org-name [name]", String, "The full name of the Chef 12 organization to be created. It must begin with a non-white space character and must be between 1 and 1023 characters long (Will ask interactively if not passed).") do |n|
        @options.full_org_name = n
      end

      # This option matches the knife -s option
      opts.on("-s", "--chef11-server-url [url]", String, "The url of the Chef 11 server.  Defaults to #{@options.chef11_server_url}") do |url|
        @options.chef11_server_url = url
      end

      opts.on("-x", "--chef12-server-url [url]", String, "The url of the Chef 12 server.  Defaults to #{@options.chef12_server_url}") do |url|
        @options.chef12_server_url = url
      end

      # This option matches the knife -u option
      opts.on("-u", "--user [user]", String, "Chef 11 API client user. This is the admin user who will be used to download the Chef 11 data. Should match with the key specified. Defaults to #{@options.chef11_admin_client_name}") do |user|
        @options.chef11_admin_client_name = user
      end

      # This option matches the knife -k option
      opts.on("-k", "--key [key]", String, "Chef 11 API client key. This is the admin key that will be used to download the Chef 11 data. Should match with the user specified. Defaults to #{@options.chef11_admin_client_key}") do |key|
        @options.chef11_admin_client_key = key
      end

      opts.on("-d", "--chef11-data-dir [directory]", String, "Directory to store open source Chef 11 server data. Defaults to a created tmp dir.") do |chef11_dir|
        @options.chef11_data_dir = chef11_dir
      end

      opts.on("-e", "--chef12-data-dir [directory]", String, "Directory where data for upload to the Chef 12 server is located. Defaults to a created tmp dir.") do |chef12_dir|
        @options.chef12_data_dir = chef12_dir
      end

      opts.on("-t", "--upload-threads [number]", Integer, "The number of threads to use when migrating cookbooks to the new server. Defaults to #{@options.upload_threads}") do |n|
        @options.upload_threads = n
      end

      # TODO(jmink) Make this work without the '--'
      opts.on("-h", "--help", "Show this message") do
        puts opts
        exit
      end
    end
    opt_parser.parse!(args)
    log "Upgrading with options #{@options.inspect}"
  end

  def detect_chef11
    # Is this reliable enough?
    File.directory?("/opt/chef-server")
  end

  def upgrade?
    if @options.skip_confirmation
      log "Performing upgrade"
      return true
    end

    log "Would you like to upgrade? [yN]"

    answer = STDIN.gets.chomp
    answer == "Y" || answer == "y"
  end

  def partybus_upgrade
    # Original Enterprise Chef upgrade path
    # Put everything in a down state except postgres before we upgrade things.
    run_command("private-chef-ctl stop")
    if reconfigure(false) != 0
      exit 1
    end
    # How upgrades should handle services:
    #  + It should expect services to be down, but turn off services
    #    if its important that they be off for the upgrade.
    #  + It should start any services it needed, and turn them off
    #    at the end of a migration.
    # with postgres being the exception to those rules. We are leaving
    # postgres up atm to avoid having to constantly restart it.
    run_command("private-chef-ctl stop")

    if running_config["private_chef"]["use_chef_backend"]
      run_command("private-chef-ctl start haproxy")
    else
      run_command("private-chef-ctl start postgresql")
    end

    sleep 15
    Dir.chdir(File.join(base_path, "embedded", "service", "partybus"))
    bundle = File.join(base_path, "embedded", "bin", "bundle")
    status = run_command("#{bundle} exec ./bin/partybus upgrade")
    if status.success?
      puts "Chef Infra Server Upgraded!"
      exit 0
    else
      exit 1
    end
  end

  ### Start script ###

  parse(ARGV)

  if detect_chef11
    log "Open source Chef 11 server detected."
    if upgrade?
      log "Upgrading the open source Chef 11 server."
      chef11_upgrade = OpenSourceChef11Upgrade.new(@options, self)
      chef11_upgrade.run_upgrade
    else
      puts "Aborting upgrade."
      exit 0
    end
  else
    # Open source Chef 11 isn't on the system, must be a partybus upgrade
    partybus_upgrade
  end

end
