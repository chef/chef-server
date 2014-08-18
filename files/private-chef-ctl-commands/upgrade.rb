#Copyright:: Copyright (c) 2012-2014 Chef Software, Inc.
#
# All Rights Reserved
#

require "/opt/opscode/embedded/service/omnibus-ctl/osc_upgrade"
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
      opts.on("-y", "--yes", "Skip confirmation") do |y|
        @options.skip_confirmation = y
      end

      opts.on("--org-name [name]", String, "The name of the Chef organization (Will ask interactively if not passed)") do |n|
        @options.org_name = n
      end

      opts.on("--full-org-name [name]", String, "The full name of the Chef organization (Will ask interactively if not passed)") do |n|
        @options.full_org_name = n
      end

      # Should this be chef-server-host to match sql-host?
      opts.on("--chef-server-url [url]", String, "The url of the chef server.  Defaults to #{@options.chef_server_url}") do |u|
         @options.chef_server_url = u
      end

      opts.on("--upload-threads [number]", Integer, "The number of threads to use when migrating cookbooks to the new server. Defaults to #{@options.upload_threads}") do |n|
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

  def detect_osc
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

  def wait_for_ready_server(server_version)
    1.upto(120) do |count|
      begin
        server_status = JSON.parse(open('http://localhost:8000/_status').read)
        fail unless server_status['status'] == 'pong'
      # Catch exceptions because if the server isn't yet up trying to open the status endpoint throws one
      rescue Exception => e
        sleep 1
        if count == 120
          log "Timeout waiting for #{server_version} server to start. Received expection #{e.message}"
          exit 1
        end
      end
    end
  end

  ### Start script ###

  parse(ARGV)

  if detect_osc
    log "Open Source Chef 11 or older server detected."
    if upgrade?
      log "Upgrading the Open Source Chef server."
      run_osc_upgrade
    else
      puts "Aborting upgrade."
      exit 0
    end
  end

  # Original EC upgrade path
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
