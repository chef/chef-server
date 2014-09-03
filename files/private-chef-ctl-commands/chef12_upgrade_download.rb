#
# Copyright:: Copyright (c) 2014 Chef Software, Inc.
#
# All Rights Reserved
#

require "/opt/opscode/embedded/service/omnibus-ctl/open_source_chef12_upgrade"
require 'optparse'
require 'ostruct'

add_command "chef12-upgrade-download", "Download data from a open source Chef 11 server.", 2 do

   def parse(args)
    @options = OpenStruct.new

    # Define defaults
    @options.chef11_server_url = "https://localhost"
    @options.chef11_admin_client_name = "admin"
    @options.chef11_admin_client_key = "/etc/chef-server/admin.pem"

    opt_parser = OptionParser.new do |opts|
      opts.banner = "Usage: private-chef-ctl chef12-upgrade-download [options]"

      opts.on("-d", "--chef11-data-dir [directory]", String, "Directory to store open source Chef 11 server data. Defaults to a created tmp dir.") do |chef11_dir|
        @options.chef11_data_dir = chef11_dir
      end

      # This option matches the knife -s option
      opts.on("-s", "--chef11-server-url [url]", String, "The url of the open source Chef 11 server.  Defaults to #{@options.chef11_server_url}") do |url|
         @options.chef11_server_url = url
      end

      # This option matches the knife -u option
      opts.on("-u", "--user [user]", String, "Chef 11 API client user. This is the admin user who will be used to download the Chef 11 data. Should match with the key specified. Defaults to #{@options.chef11_admin_client_name}") do |user|
        @options.chef11_admin_client_name = user
      end

      # This option matches the knife -k option
      opts.on("-k", "--key [key]", String, "Chef 11 API client key. This is the admin key that will be sued to download the Chef 11 data. Should match with the user specified. Defaults to #{@options.chef11_admin_client_key}") do |key|
        @options.chef11_admin_client_key = key
      end

      opts.on("-h", "--help", "Show this message") do
        puts opts
        exit
      end
    end

    opt_parser.parse!(args)
    log "Proceeding with options #{@options.inspect}"
   end

  ### Start script ###

  parse(ARGV)

  chef11_upgrade = OpenSourceChef11Upgrade.new(@options, self)
  chef11_data_dir = chef11_upgrade.determine_chef11_data_dir
  key_file = "#{chef11_data_dir}/key_dump.json"
  chef11_upgrade.download_chef11_data(chef11_data_dir, key_file)

end
