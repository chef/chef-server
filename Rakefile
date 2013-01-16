require 'bundler/setup'
require 'omnibus'

Omnibus.setup do |o|
  ##
  # Config Section
  ##
  o.config.install_dir = '/opt/opscode'

  Omnibus::S3Tasks.define!
  Omnibus::CleanTasks.define!
end

overrides = Omnibus::Overrides.overrides

Omnibus.projects("config/projects/*.rb")

local_defs = Rake::FileList["config/software/*.rb"]
omnibus_software_defs = Rake::FileList[File.join(Bundler.definition.specs["omnibus-software"][0].gem_dir, "config/software/*.rb")]

# ignore software definitions from omnibus-software that exist in project repo
project_defs = local_defs + omnibus_software_defs.delete_if do |software|
  local_defs.include?(software.gsub(/.*(config\/software\/.*\.rb)/, '\1'))
end

Omnibus.software(
  overrides,
  *project_defs
)

desc "Print the name and version of all components"
task :versions do
  puts Omnibus::Reports.pretty_version_map
end
