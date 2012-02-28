require 'omnibus'

Omnibus.setup

Omnibus::S3Tasks.define!

##
# Config Section
##
Omnibus.config.install_dir = '/opt/opscode'

Omnibus.software("../omnibus-ruby/config/software/*.rb", "config/software/*.rb")
Omnibus.projects("config/projects/*.rb")

desc "Print the name and version of all components"
task :versions do
  puts Omnibus::Reports.pretty_version_map
end

# Build control tasks
begin
  require 'vagrant'
  have_vagrant = true
rescue LoadError => e
  have_vagrant = false
end

if have_vagrant
  vagrant = Vagrant::Environment.new(:ui_class => Vagrant::UI::Colored)
  namespace :vagrant do

    desc "Boot a build VM with Vagrant"
    task :up do
      if vagrant.primary_vm.created?
        vagrant.primary_vm.start
        vagrant.primary_vm.provision
      else
        vagrant.primary_vm.up
      end
    end

    desc "Boot a build VM with vagrant and run a build"
    task :build => :up do
      vagrant.primary_vm.channel.execute("cd opscode-omnibus && bundle install && bundle exec rake projects:private-chef:deb --trace") do |stream, data|
        out = stream == :stdout ? $stdout : $stderr
        out.puts(data)
      end
    end
  end
end
