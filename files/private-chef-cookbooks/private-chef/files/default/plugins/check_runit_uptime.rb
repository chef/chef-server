#!/usr/bin/ruby
# 
# Author:: Nathan Haneysmith <nathan@opscode.com>

# check-runit-uptime.rb
# this script doesn't care if the service is down
# only if it's been running too long...
 
require 'rubygems'

service = "#{ARGV[0]}"
warn = "#{ARGV[1]}".to_i # hours uptime to warn
crit = "#{ARGV[2]}".to_i # hours uptime to crit

if ARGV[2].nil?
  puts "#{$0} by Nathan Haneysmith <nathan@opscode.com> Stephen Delano <stephen@opscode.com>\n"
  puts "Nagios plugin to check runit uptime\n"
  puts "Usage: #{$0} <runit service> <hours to warn> <hours to crit>\n"
  puts "e.g. #{$0} couchdb 7 14\n"
  exit 3
end

# hours in current status
state = `/usr/bin/sv status #{service} | awk -F: '{print $1}'`.chomp
time_in_current_state = `/usr/bin/sv status #{service} | awk '{printf "%.0f",$5/3600}'`.to_i

# service unconfigured?
if state == "fail"
  puts "UNKNOWN: #{service} status check failed, check 'sv status #{service}'\n"
  exit 3
end

# maybe a future version will have logic around what to do if not running
# for now, down equals "not running too long"
if state != "run"
  puts "OK: #{service} not running\n"
  exit 0
end

if time_in_current_state > crit
  puts "CRITCAL: #{service} uptime #{time_in_current_state} hours > #{crit}\n"
  exit 2
elsif time_in_current_state > warn
  puts "WARNING: #{service} uptime #{time_in_current_state} hours > #{warn}\n"
  exit 1
else
  puts "OK: #{service} uptime #{time_in_current_state} hours < #{warn}\n"
  exit 0
end

