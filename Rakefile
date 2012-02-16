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
  vagrant = Vagrant::Environment.new
  namespace :vagrant do
    task :boxes do
      # Mixlib::ShellOut.new("vagrant box add lucid64 http://files.vagrantup.com/lucid64.box", :live_stream => STDOUT).run_cmmand
      vagrant.cli("box add lucid64 http://files.vagrantup.com/lucid64.box")
    end

    task :up => :boxes do
      vagrant.cli("up")
    end

    task :build => :up do
      vagrant.primary_vm.channel.execute("cd opscode-omnibus && rake projects:opscode-webui") do |stream, data|
        out = stream == :stdout ? $stdout : $stderr
        out.puts(data)
      end
    end
  end
end
