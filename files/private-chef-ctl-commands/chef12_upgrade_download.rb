#
# Copyright:: Copyright (c) 2014 Chef Software, Inc.
#
# All Rights Reserved
#

require "/opt/opscode/embedded/service/omnibus-ctl/osc_upgrade"
require 'optparse'
require 'ostruct'

add_command "chef12-upgrade-download", "Download data from a Chef 11 server.", 2 do

   def parse(args)
    @options = OpenStruct.new

    # Define defaults
    @options.chef_server_url = "https://localhost"

    opt_parser = OptionParser.new do |opts|
      opts.banner = "Usage: private-chef-ctl chef12-upgrade-download [options]"

      opts.on("-d", "--data-dir [directory]", "Directory to store Chef 11 data. Defaults to a created tmp dir.") do |chef11_dir|
        @options.chef11_data_dir = chef11_dir
      end

      opts.on("-s", "--chef-server-url [url]", String, "The url of the Chef 11 server.  Defaults to #{@options.chef_server_url}") do |u|
         @options.chef_server_url = u
      end

      opts.on("-h", "--help", "Show this message") do
        puts opts
        exit
      end
    end

    opt_parser.parse!(args)
    log "Proceeding with options #{@options.inspect}"
   end

   def determine_chef11_data_dir
    if @options.chef11_data_dir
      @options.chef11_data_dir
    else
      chef11_data_dir = Dir.mktmpdir('chef11-chef-server-data')
      log "Creating #{chef11_data_dir} as the location to save the Chef 11 server data"
      chef11_data_dir
    end
   end

  ### Start script ###

  parse(ARGV)

  chef11_data_dir = determine_chef11_data_dir
  key_file = "#{chef11_data_dir}/key_dump.json"

  osc_upgrade = OscUpgrade.new(@options, self)
  osc_upgrade.download_osc_data(chef11_data_dir, key_file)

  log "Chef 11 server data downloaded to #{chef11_data_dir}"
end
