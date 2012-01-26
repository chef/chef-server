$:.unshift File.expand_path("../../omnibus/lib", __FILE__)

require 'omnibus'

FileList["../omnibus/config/software/*.rb",
         "config/software/*.rb"].each do |f|
  Omnibus::Software.new(IO.read(f))
end

FileList["config/projects/*.rb"].each do |f|
  Omnibus::Project.new(IO.read(f))
end
