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

require 'optparse'
require 'chef/config'
require 'chef/org'
require 'redis'

def all_orgs
  Chef::Config.from_file(::ChefServerCtl::Config.knife_config_file)
  Chef::Org.list.keys.sort
end

def reindex_data_for_org(org)
  reindex_script = ::ChefServerCtl::Config.erchef_reindex_script
  lb_url = ::ChefServerCtl::Config.lb_url
  status = run_command("#{reindex_script} complete #{org} #{lb_url}")
  if !status.success?
    $stderr.puts "Failed to reindex data for #{org}!"
  end
end

def redis
  @redis ||= begin
               vip = running_config["private_chef"]["redis_lb"]["vip"]
               port = running_config["private_chef"]["redis_lb"]["port"]
               password = credentials.get('redis_lb', 'password')
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

add_command_under_category "reindex", "general", "Reindex all server data for a given organization", 2 do
  reindex_args = ARGV[1..-1] # Chop off first 1 args, keep the rest... that is, everything after "private-chef-ctl reindex"
  options = {}

  OptionParser.new do |opts|

    # NOTE(ssd) 2018-08-09: --wait and --disable-api aren't currently
    # supported in either of the Habitat packages we offer.
    if !::ChefServerCtl::Config.habitat_mode
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

  do_reindex(orgs_to_reindex, options)

  if options[:with_timing]
    puts "#{Time.now - start_time} seconds to reindex."
    exit 0
  end
end
