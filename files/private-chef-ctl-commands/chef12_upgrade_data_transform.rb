#
# Copyright:: Copyright (c) 2014 Chef Software, Inc.
#
# All Rights Reserved
#

require "/opt/opscode/embedded/service/omnibus-ctl/osc_upgrade"
require 'optparse'
require 'ostruct'

add_command "chef12-upgrade-data-transform", "Transfrom data from a Chef 11 server for upload to an Chef 12 server.", 2 do

   def parse(args)
    @options = OpenStruct.new

    # Define defaults
    @options.chef_server_url = "https://localhost"

    opt_parser = OptionParser.new do |opts|
      opts.banner = "Usage: private-chef-ctl chef12-upgrade-data-transform [options]"

      opts.on("-d", "--data-dir [directory]", "Directory of Chef 11 data. (Will ask interactively if not passed)") do |chef11_dir|
        @options.chef11_data_dir = chef11_dir
      end

      opts.on("-t", "--transformed-data-dir [directory]", "Directory to place transformed data. Defaults to a tmp dir.") do |chef12_dir|
        @options.chef12_data_dir = chef12_dir
      end

      opts.on("-o", "--org-name [name]", String, "The name of the Chef organization (Will ask interactively if not passed)") do |n|
        @options.org_name = n
      end

      opts.on("-f", "--full-org-name [name]", String, "The full name of the Chef organization (Will ask interactively if not passed)") do |n|
        @options.full_org_name = n
      end

      opts.on("-h", "--help", "Show this message") do
        puts opts
        exit
      end
    end

    opt_parser.parse!(args)
   end

   def determine_chef12_data_dir
    if @options.chef12_data_dir
      @options.chef12_data_dir
    else
      chef12_dir = Dir.mktmpdir('ec-chef-server-data')
      log "Created #{chef12_dir} as the location to save the tranformed data"
      chef12_dir
    end
   end


  ### Start script ###

  parse(ARGV)

  # Check if this is a valid directory and bail if it isn't
  chef11_data_dir = @options.chef11_data_dir || ask("Location of Chef 11 server data? ")
  key_file = "#{chef11_data_dir}/key_dump.json"
  chef12_data_dir = determine_chef12_data_dir

  osc_upgrade = OscUpgrade.new(@options, self)
  osc_upgrade.transform_osc_data(chef11_data_dir, key_file, chef12_data_dir)

  log "Data transformed and saved to #{chef12_data_dir}"
end
