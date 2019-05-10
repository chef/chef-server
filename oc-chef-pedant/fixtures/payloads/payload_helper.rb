#!/usr/bin/ruby
#

#
# Dumb utility to regenerate payload files in this directory when whe change the max post size parameter
#

STRING = "abcdefghijklmnopqrstuvwxyz123456"

length = ARGV[0].to_i || 200000
file = ARGV[1] || "outfile.json"

puts "Writing #{length} bytes to #{file}"

PREFIX = '\'["'
SUFFIX = '"]\''


File.open(file, "w") do |f|
  f.write(PREFIX)
  slength = length - PREFIX.length - SUFFIX.length
  f.write(STRING * (slength / STRING.length))
  f.write(STRING.slice(0, slength % STRING.length))
  f.write(SUFFIX)
#  f.write("\n")
end
