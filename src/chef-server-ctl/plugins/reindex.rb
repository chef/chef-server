#
# Copyright 2015 Chef Software, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

require "optparse"
require "chef/config"
require "chef/org"
require "redis"
require "chef_server_ctl/helpers/du"
require "chef_server_ctl/helpers/statfs"

def all_orgs
  Chef::Config.from_file(::ChefServerCtl::Config.knife_config_file)
  Chef::Org.list.keys.sort
end

def reindex_data_for_org(org)
  reindex_script = ::ChefServerCtl::Config.erchef_reindex_script
  lb_url = ::ChefServerCtl::Config.lb_url
  status = run_command("#{reindex_script} complete #{org} #{lb_url}")
  unless status.success?
    $stderr.puts "Failed to reindex data for #{org}!"
  end
end

def redis
  @redis ||= begin
               vip = running_config["private_chef"]["redis_lb"]["vip"]
               port = running_config["private_chef"]["redis_lb"]["port"]
               password = credentials.get("redis_lb", "password")
               Redis.new(port: port, ip: vip, password: password)
             end
end

def disable_api
  puts "- Disabling the Chef API."
  redis.hset("dl_default", "503_mode", true)
end

def enable_api
  puts "- Re-enabling the Chef API"
  redis.hdel("dl_default", "503_mode")
end

def do_reindex(orgs_to_reindex, options)
  disable_api if options[:disable_api]

  puts "- Reindexing."
  orgs_to_reindex.each do |org|
    reindex_data_for_org(org)
  end
ensure
  enable_api if options[:disable_api]
end

# Disk space verification for running reindex
# It needs enough disk space to hold a second copy of the data plus whatever change in size the new index might need if the schema has changed.
# Required Disk Space = (2 * size of ES data directory)
def verify_disk_space
  data_dir = running_config["private_chef"]["elasticsearch"]["data_dir"]
  if Dir.exist?(data_dir)
    data_dir_size = Du.du(data_dir)
    free_disk_space = Statfs.new("#{data_dir}/..").free_space
    if (2.2 * data_dir_size) < free_disk_space # The minimum space should be double the existing data size
      puts "Free space is sufficient to start Elasticsearch reindex"
    else
      $stderr.puts "Insufficient free space on disk to complete reindex."
      $stderr.puts "The current elasticsearch data directory contains #{data_dir_size} KB of data but only #{free_disk_space} KB is available on disk."
      $stderr.puts "The reindex process requires at least #{2.2 * data_dir_size} KB."
      exit 1
    end
  else
    puts "Elasticsearch data path does not exist, so skipping the disk space verification: #{data_dir}"
  end
rescue => exception
  puts "Skipping the disk space verification due to #{exception.message}"
end

add_command_under_category "reindex", "general", "Reindex all server data for a given organization", 2 do
  reindex_args = ARGV[1..-1] # Chop off first 1 args, keep the rest... that is, everything after "private-chef-ctl reindex"
  options = {}

  OptionParser.new do |opts|

    # NOTE(ssd) 2018-08-09: --wait and --disable-api aren't currently
    # supported in either of the Habitat packages we offer.
    unless ::ChefServerCtl::Config.habitat_mode
      opts.on("-w", "--wait", "Legacy option to wait for indexing queue to empty. This option does nothing.") do |w|
        $stderr.puts "Ignoring wait option as rabbitmq-based indexing is no longer supported"
        options[:wait] = false
      end

      opts.on("-d", "--disable-api", "Disable writes during reindexing") do |n|
        options[:disable_api] = n
      end
    end

    opts.on("-a", "--all-orgs", "Reindex all organizations. Overrides any organizations provided as arguments.") do |a|
      options[:all_orgs] = a
    end

    opts.on("-t", "--with-timing", "Print reindex timing information") do |a|
      options[:with_timing] = a
    end
  end.parse!(reindex_args)

  if options[:with_timing]
    start_time = Time.now
  end

  orgs_to_reindex = if options[:all_orgs]
                      puts "Reindexing all organizations"
                      all_orgs
                    else
                      puts "Reindexing orgs: #{reindex_args.compact}"
                      reindex_args.compact
                    end

  if orgs_to_reindex.length == 0 && options[:all_orgs]
    puts "No organizations to reindex"
    exit 0
  elsif orgs_to_reindex.length == 0
    $stderr.puts "Please specify an organization to reindex or use the --all-orgs flag"
    exit 1
  end

  # Checking free disk space before proceeding to reindex
  verify_disk_space

  do_reindex(orgs_to_reindex, options)

  if options[:with_timing]
    puts "#{Time.now - start_time} seconds to reindex."
    exit 0
  end
end
