$:.unshift File.expand_path("../../omnibus/lib", __FILE__)

require 'omnibus'

FileList["../omnibus/config/software/*.rb",
         "config/software/*.rb"].each do |f|gi8t
  Omnibus::Software.new(IO.read(f))
end
