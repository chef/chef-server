#
# Copyright:: Copyright (c) 2014 Chef Software, Inc.
#
# All Rights Reserved
#

require "/opt/opscode/embedded/service/omnibus-ctl/open_source_chef12_upgrade"
require 'optparse'
require 'ostruct'

add_command "chef12-upgrade-upload", "Upload transformed open source Chef 11 data to a Chef 12 server.", 2 do

    def parse(args)
      @options = OpenStruct.new

      # Define defaults
      @options.chef12_server_url = "https://localhost"
      @options.upload_threads = 10

      opt_parser = OptionParser.new do |opts|
        opts.banner = "Usage: chef-server-ctl chef12-upgrade-upload [options]"

        opts.on("-e", "--chef12-data-dir [directory]", String, "Directory where data for upload to the Chef 12 server is located (Will ask interactively if not passed)") do |chef12_dir|
          @options.chef12_data_dir = chef12_dir
        end

        opts.on("-x", "--chef12-server-url [url]", String, "The url of the Chef 12 server.  Defaults to #{@options.chef12_server_url}") do |url|
          @options.chef12_server_url = url
        end

        opts.on("-t", "--upload-threads [number]", Integer, "The number of threads to use when migrating cookbooks to the new server. Defaults to #{@options.upload_threads}") do |n|
          @options.upload_threads = n
        end

        opts.on("-o", "--org-name [name]", String, "The name of the default Chef 12 organization to be applied. This is needed so existing users and clients will continue to work. This should be the same as the org name supplied when migrating data. (Will ask interactively if not passed).") do |n|
        @options.org_name = n
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

  chef12_data_dir = @options.chef12_data_dir || ask("Location of data to upload to Chef 12 server?")
  org_name = @options.org_name || ask("Chef 12 short organization name? ")

  chef11_upgrade = OpenSourceChef11Upgrade.new(@options, self)
  chef11_upgrade.validate_org_name(org_name)
  chef11_upgrade.set_default_chef12_config(org_name)
  chef11_upgrade.upload_transformed_data(chef12_data_dir)
  chef11_upgrade.adjust_node_permissions(chef12_data_dir)

end
