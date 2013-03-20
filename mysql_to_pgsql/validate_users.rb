#!/usr/bin/env ruby

require 'rubygems'
require 'pg'
require 'yajl'

db_password = ARGV[0]
unless db_password
  `stty -echo` # turn off echo for #gets
  print "Password for opscode_chef: "
  db_password = gets.chomp
  puts         # print output on the next line
  `stty echo`
end

errors = total = 0

conn = PG.connect({:dbname => 'opscode_chef',
    :host => 'localhost',
    :user => 'opscode_chef',
    :password => db_password
  })
conn.exec("select username, serialized_object from users") do |result|
  result.each do |row|
    total += 1 # really? no #size function?
    begin
      Yajl::Parser.parse(row['serialized_object'])
    rescue Yajl::ParseError => e
      errors += 1
      puts "Error parsing user '#{row['username']}':"
      puts e.message
      puts
    end
  end
end

if errors > 0
  puts "Validation Failed: #{errors} / #{total} rows."
  exit 1
else
  puts "Validaton Successful: #{total} rows."
end
