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

      opts.on("-d", "--data-dir [directory]", "Directory of Chef 11 data. (Will ask interactively if not passed)") do |dir|
        @options.osc_data_dir = dir
      end

      opts.on("-t", "--transformed-data-dir [directory]", "Directory to place transformed data. Defaults to a tmp dir.") do |ec_dir|
        @options.ec_data_dir = ec_dir
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

   def data_dir
    if @options.ec_data_dir
      @options.ec_data_dir
    else
      ec_dir = Dir.mktmpdir('ec-chef-server-data')
      log "Created #{ec_dir} as the location to save the tranformed data"
      ec_dir
    end
   end


  ### Start script ###

  parse(ARGV)

  # Check if this is a valid directory and bail if it isn't
  osc_data_dir = @options.osc_data_dir || ask("Location of Chef 11 server data? ")
  key_file = "#{osc_data_dir}/key_dump.json"
  ec_data_dir = data_dir

  osc_upgrade = OscUpgrade.new(@options, self)
  osc_upgrade.transform_osc_data(osc_data_dir, key_file, ec_data_dir)

  log "Data transformed and saved to #{ec_data_dir}"
end
