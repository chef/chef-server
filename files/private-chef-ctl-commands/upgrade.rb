#Copyright:: Copyright (c) 2012-2014 Chef Software, Inc.
#
# All Rights Reserved
#

require "/opt/opscode/embedded/service/omnibus-ctl/open_source_chef12_upgrade"
require 'optparse'
require 'ostruct'

add_command "upgrade", "Upgrade your private chef installation.", 2 do

  # Since this is evaled, need to have methods first so they can be picked up

  def parse(args)
    @options = OpenStruct.new

    # Define defaults
    @options.skip_confirmation = false
    @options.chef_server_url = "https://localhost"
    @options.upload_threads = 10

    opt_parser = OptionParser.new do |opts|
      opts.banner = "Usage: private-chef-ctl upgrade [options]"
      opts.banner = opts.banner << "\n Options only apply to open source Chef 11 server to Chef 12 server upgrades."
      opts.banner = opts.banner << "\n If upgrading from Enterprise Chef 11 server to Chef 12 server no options are needed."

      opts.on("-y", "--yes", "Skip confirmation") do |y|
        @options.skip_confirmation = y
      end

      opts.on("-o", "--org-name [name]", String, "The name of the Chef organization (Will ask interactively if not passed)") do |n|
        @options.org_name = n
      end

      opts.on("-f", "--full-org-name [name]", String, "The full name of the Chef organization (Will ask interactively if not passed)") do |n|
        @options.full_org_name = n
      end

      # Should this be chef-server-host to match sql-host?
      opts.on("-s", "--chef-server-url [url]", String, "The url of the chef server.  Defaults to #{@options.chef_server_url}") do |u|
         @options.chef_server_url = u
      end

      opts.on("-u", "--upload-threads [number]", Integer, "The number of threads to use when migrating cookbooks to the new server. Defaults to #{@options.upload_threads}") do |n|
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
    return answer == 'Y' || answer == 'y'
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
  end

  # Original Enterprise Chef upgrade path
  reconfigure(false)
  # Put everything in a down state before we upgrade things.
  # How upgrades should handle services:
  #  + It should expect services to be down, but turn off services
  #    if its important that they be off for the upgrade.
  #  + It should start any services it needed, and turn them off
  #    at the end of a migration.
  run_command("private-chef-ctl stop")
  Dir.chdir(File.join(base_path, "embedded", "service", "partybus"))
  bundle = File.join(base_path, "embedded", "bin", "bundle")
  status = run_command("#{bundle} exec ./bin/partybus upgrade")
  if status.success?
    puts "Chef Server Upgraded!"
    exit 0
  else
    exit 1
  end

end
