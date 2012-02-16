#!/usr/bin/env ruby
# Author:: Bryan McLellan <btm@loftninjas.org>
#
# Copyright 2010, Opscode, Inc
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

require 'xmlsimple'
require 'uri'
require 'net/http'
require 'optparse'
require 'date'

# Set default options
options = {}
options[:hostname] = "localhost"
options[:port] = "8983"
options[:warn] = "15"
options[:crit] = "30"

# Parse command line options
opts = OptionParser.new
opts.on('-H', '--hostname [hostname]', 'Host to connect to [localhost]') do |hostname|
  options[:hostname] = hostname
end
  
opts.on('-p', '--port [port]', 'Port to connect to [8983]') do |port|
  options[:port] = port
end

opts.on('-w', '--warn [minutes]', 'Threshold for warning [15]') do |warn|
  options[:warn] = warn
end

opts.on('-c', '--crit [minutes]', 'Threshold for critical [30]') do |crit|
  options[:crit] = crit
end

opts.on( '-h', '--help', 'Display this screen' ) do
  puts opts
  exit 3
end
opts.parse!

# Fetch replication data from slave in XML  
res = Net::HTTP.start(options[:hostname], options[:port]) do |http|
  http.get("/solr/replication?command=details")
end

unless res.code == "200"
  puts "CRITICAL - Unable to contact slave: HTTP #{res.code}"
  exit 2
end 

# Parse master host from slaves configuration
slave_solr = XmlSimple.xml_in(res.body, { 'KeyAttr' => 'name'})
master_url = URI.parse(slave_solr["lst"]["details"]["lst"]["slave"]["str"]["masterUrl"]["content"])

# Fetch replication data from master in XML
res = Net::HTTP.start(master_url.host, master_url.port) do |http|
  http.get("/solr/replication?command=details")
end

unless res.code == "200"
  puts "CRITICAL - Unable to contact master: HTTP #{res.code}"
  exit 2
end 

master_solr = XmlSimple.xml_in(res.body, { 'KeyAttr' => 'name'})

# Parse returned data in a hash
master = Hash.new
slave = Hash.new
master['index_version'] = master_solr["lst"]["details"]["long"]["indexVersion"]["content"]
slave['index_version'] = slave_solr["lst"]["details"]["long"]["indexVersion"]["content"]
slave['replicated_time'] = slave_solr["lst"]["details"]["lst"]["slave"]["arr"]["indexReplicatedAtList"]["str"].first

# First check to see if we have matching index numbers
# If they don't match, how long since we have succeeded?
current_time = DateTime.now
replication_time= DateTime.parse(slave['replicated_time'])
if master['index_version'] == slave['index_version']
  puts "OK - Slave is current: #{replication_time}" 
  exit 0
elsif replication_time < current_time - options[:crit].to_i / 1440.0
  puts "CRITICAL - Slave more than #{options[:crit]} minutes old: #{replication_time}"
  exit 2
elsif replication_time < current_time - options[:warn].to_i / 1440.0
  puts "WARNING - Slave more than #{options[:warn]} minutes old: #{replication_time}"
  exit 1
elsif replication_time > current_time - options[:warn].to_i / 1440.0
  puts "OK - Slave is recent: #{replication_time}"
  exit 0
else
  puts "UNKNOWN - Unable to determine replication status"
  exit 3
end

