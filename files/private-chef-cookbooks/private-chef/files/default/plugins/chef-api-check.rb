#!/usr/bin/ruby
# 
# Author:: Nathan Haneysmith <nathan@opscode.com>
#

# check-chef-api.rb
# -s URI (ie: https://api.opscode.com/organizations/opscode-www )
# -k KEY (path to client_key (nathan-corpsite.pem file))
# -u USER (API Client Username)
# -t TASK (node, client, search)

uri = ARGV[0]
key = ARGV[1]
user = ARGV[2]
task = ARGV[3]

subcommand = case task
  when "node" then "node list"
  when "client" then "client list"
  when "search" then "search node hostname:ubuntu"
  else "Unknown"
end

if ARGV[3].nil? || subcommand == "Unknown"
  puts "#{0} by Nathan Haneysmith <nathan@opscode.com>\n"
  puts "Nagios plugin to run chef-client against server/api\n"
  puts "Usage: ${0} <server url> <path to api key> <api username> <task>\n"
  puts "Task can be node (node list), client (client list), or search\n"
  puts "e.g. #{0} https://api.opscode.com /tmp/nagios.pem nagios node\n"
  exit 3
end

result = %x[knife #{subcommand} -s #{uri} -k #{key} -u #{user}]
puts result

puts "Now do stuff with the result to verify and return a nagios consumable result with appropriate exit codes\n"

