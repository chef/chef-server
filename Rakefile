$:.unshift File.expand_path("../../omnibus-ruby/lib", __FILE__)

require 'omnibus'

FileList["../omnibus-ruby/config/software/*.rb",
         "config/software/*.rb"].each do |f|
  Omnibus::Software.new(IO.read(f))
end

FileList["config/projects/*.rb"].each do |f|
  Omnibus::Project.new(IO.read(f))
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
    task :up do
      if vagrant.primary_vm.created?
        vagrant.primary_vm.start
        vagrant.primary_vm.provision
      else
        vagrant.primary_vm.up
      end
    end

    task :build => :up do
      vagrant.primary_vm.channel.execute("cd opscode-omnibus && rake projects:opscode-webui:deb --trace") do |stream, data|
        out = stream == :stdout ? $stdout : $stderr
        out.puts(data)
      end
    end
  end
end
