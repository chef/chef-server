# -*- mode: ruby -*-
# vi: set ft=ruby :

require "vagrant"

if Vagrant::VERSION < "1.2.1"
  raise "The Omnibus Build Lab is only compatible with Vagrant 1.2.1+"
end

host_project_path = File.expand_path("..", __FILE__)
guest_project_path = "/home/vagrant/#{File.basename(host_project_path)}"
project_name = "private-chef"

Vagrant.configure("2") do |config|

  config.vm.hostname = "#{project_name}-omnibus-build-lab"

  config.vm.define 'ubuntu-10.04' do |c|
    c.vm.box = "opscode-ubuntu-10.04"
    c.vm.box_url = "https://opscode-vm.s3.amazonaws.com/vagrant/opscode_ubuntu-10.04_provisionerless.box"
  end

  config.vm.define 'ubuntu-12.04' do |c|
    c.vm.box = "opscode-ubuntu-12.04"
    c.vm.box_url = "https://opscode-vm.s3.amazonaws.com/vagrant/opscode_ubuntu-12.04_provisionerless.box"
  end

  config.vm.define 'centos-5' do |c|
    c.vm.box = "opscode-centos-5.9"
    c.vm.box_url = "https://opscode-vm.s3.amazonaws.com/vagrant/opscode_centos-5.9_provisionerless.box"
  end

  config.vm.define 'centos-6' do |c|
    c.vm.box = "opscode-centos-6.4"
    c.vm.box_url = "https://opscode-vm.s3.amazonaws.com/vagrant/opscode_centos-6.4_provisionerless.box"
  end

  config.vm.provider :virtualbox do |vb|
    # Give enough horsepower to build without taking all day.
    vb.customize [
      "modifyvm", :id,
      "--memory", "1536",
      "--cpus", "2",
    ]
  end

  # Ensure a recent version of the Chef Omnibus packages are installed
  config.omnibus.chef_version = "11.4.4"

  # Enable the berkshelf-vagrant plugin
  config.berkshelf.enabled = true
  # The path to the Berksfile to use with Vagrant Berkshelf
  config.berkshelf.berksfile_path = "./Berksfile"

  config.ssh.max_tries = 40
  config.ssh.timeout   = 120
  config.ssh.forward_agent = true

  config.vm.synced_folder host_project_path, guest_project_path

  # config.vm.synced_folder File.expand_path("../../omnibus-software", __FILE__), "/home/vagrant/omnibus-software"
  # config.vm.synced_folder File.expand_path("../../omnibus-ruby", __FILE__), "/home/vagrant/omnibus-ruby"

  config.vm.provision :chef_solo do |chef|
    chef.json = {
      "omnibus" => {
        "build_user" => "vagrant",
        "install_dir" => "/opt/opscode"
      }
    }

    chef.run_list = [
      "recipe[omnibus::default]"
    ]
  end

  config.vm.provision :shell, :inline => <<-OMNIBUS_BUILD
    export PATH=/usr/local/bin:$PATH
    cd #{guest_project_path}
    su vagrant -c "bundle install --binstubs"
    su vagrant -c "bin/omnibus build project #{project_name}"
  OMNIBUS_BUILD
end
