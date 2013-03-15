#!/usr/bin/env ruby

require 'rubygems'
require 'pg'
require 'yajl'
require 'pp'

errors = false
conn = PG.connect(dbname: 'opscode_chef', user: 'opscode_chef')
conn.exec("select username, serialized_object from users") do |result|
  result.each do |row|
    begin
      Yajl::Parser.parse(row['serialized_object'])
    rescue Yajl::ParseError => e
      errors = true
      puts "Error parsing user '#{row['username']}':"
      puts e.message
      puts
    end
  end
end

if errors
  exit 1
end
