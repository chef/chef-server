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
    @options.interactive = false
    @options.chef_server_url = "localhost"
    @options.sql_host = "localhost"
    @options.sql_port = 5432

    # overrides for testing:
    #@options.chef_server_url = 'https://api.opscode.piab'
    #@options.org_name = 'minitrue'
    #@options.full_org_name = "MinistryOfTruth"
    #@options.org_type = "Business"

    opt_parser = OptionParser.new do |opts|
      opts.on("-i", "--interactive", "Run interactively, as opposed to requiring all arguments from the command line") do |i|
        @options.interactive = i
      end

      opts.on("--org-name", "The name of the Chef organization (Required for non-interactive mode)") do |n|
        @options.org_name = n
      end

      opts.on("--full-org-name", "The full name of the Chef organization (Required for non-interactive mode)") do |n|
        @options.full_org_name = n
      end

      # TODO(jmink) restrict with same enum as in determine_org_name
      opts.on("--org-type","The type of the Chef organization for example Buisness (Required for non-interactive mode)") do |t|
        @options.org_type = t
      end

      # Should this be chef-server-host to match sql-host?
      opts.on("--chef-server-url", "The url of the chef server.  Defaults to #{@options.chef_server_url}") do |u|
         @options.chef_server_url = u
      end

      opts.on("--sql_host", "The url of the sql server.  Defaults to #{@options.sql_host}") do |h|
        @options.sql_host = h
      end

      opts.on("--sql_port", "The port of the sql server.  Defaults to #{@options.sql_port}") do |p|
        @options.sql_port = p
      end

      # TODO(jmink) Make this work without the --
      opts.on("-h", "--help", "Show this message") do
        puts opts
        exit
      end
    end
    opt_parser.parse!(args)

    unless @options.interactive
      if @options.org_name.nil? || @options.full_org_name.nil? || @options.org_type.nil?
        log "org-name, full-org-name and org-type are all required unless --interactive is used.  Run --help for more information"
        # Do we want to exit 0 for instances where the server wasn't upgraded?
        exit 0
      end
    end
    @options
  end

  def detect_osc
    # Is this reliable enough?
    File.directory?("/opt/chef-server")
  end

  def upgrade?
    if @options.interactive
      log "Would you like to upgrade? [yN]"

      answer = STDIN.gets.chomp
      return answer == 'Y' || answer == 'y'
    else
      # TODO(jmink) Do we want a sleep here?
      log "Performing upgrade"
      return true
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
  Dir.chdir(File.join(base_path, "embedded", "service", "partybus"))
  bundle = File.join(base_path, "embedded", "bin", "bundle")
  # Why are we calling upgrade twice?
  status = run_command("echo 'Sleeping for 2 minutes before migration' ; sleep 120 ; #{bundle} exec ./bin/partybus upgrade")
  status = run_command("#{bundle} exec ./bin/partybus upgrade")
  if status.success?
    puts "Chef Server Upgraded!"
    exit 0
  else
    exit 1
  end

end
