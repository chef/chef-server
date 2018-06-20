#
# Copyright:: Copyright (c) 2014 Chef Software, Inc.
#
# All Rights Reserved
#

# TODO refactor this for gemified chef-server-ctl
require_relative "open_source_chef12_upgrade"
require 'optparse'
require 'ostruct'

add_command_under_category "chef12-upgrade-data-transform", "open-source-upgrade", "Transfrom data from an open source Chef 11 server for upload to an Chef 12 server.", 2 do

   def parse(args)
    @options = OpenStruct.new

    opt_parser = OptionParser.new do |opts|
      opts.banner = "Usage: chef-server-ctl chef12-upgrade-data-transform [options]"

      opts.on("-d", "--chef11-data-dir [directory]", String, "Directory of open source Chef 11 server data. (Will ask interactively if not passed)") do |chef11_dir|
        @options.chef11_data_dir = chef11_dir
      end

      opts.on("-e", "--chef12-data-dir [directory]", String, "Directory to place transformed data. Defaults to a tmp dir.") do |chef12_dir|
        @options.chef12_data_dir = chef12_dir
      end

      opts.on("-o", "--org-name [name]", String, "The name of the Chef 12 organization to be created. It must begin with a lower case letter or digit; can only have lower case letters, digits, hyphens, and underscores and must be between 1 and 255 characters long (Will ask interactively if not passed).") do |n|
        @options.org_name = n
      end

      opts.on("-f", "--full-org-name [name]", String, "The full name of the Chef 12 organization to be created. It must begin with a non-white space character and must be between 1 and 1023 characters long (Will ask interactively if not passed).") do |n|
        @options.full_org_name = n
      end

      opts.on("-h", "--help", "Show this message") do
        puts opts
        exit
      end
    end

    opt_parser.parse!(args)
   end


  ### Start script ###

  parse(ARGV)

  # Check if this is a valid directory and bail if it isn't
  chef11_data_dir = @options.chef11_data_dir || ask("Location of open source Chef 11 server data? ")
  key_file = "#{chef11_data_dir}/key_dump.json"

  chef11_upgrade = OpenSourceChef11Upgrade.new(@options, self)

  org_name, org_full_name = chef11_upgrade.determine_org_name
  chef11_upgrade.validate_org_names(org_name, org_full_name)

  chef12_data_dir = chef11_upgrade.determine_chef12_data_dir
  chef11_upgrade.transform_chef11_data(chef11_data_dir, key_file, chef12_data_dir, org_name, org_full_name)

end
