# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant::Config.run do |config|

  config.vm.define 'ubuntu-10.04' do |c|
    c.vm.box     = "lucid64"
    c.vm.box_url = "http://files.vagrantup.com/lucid64.box"
  end

  config.vm.define 'centos-6.0' do |c|
    c.vm.box     = "opscode-centos-6.0"
  end

  # Share an additional folder to the guest VM. The first argument is
  # an identifier, the second is the path on the guest to mount the
  # folder, and the third is the path on the host to the actual folder.
  # config.vm.share_folder "v-data", "/vagrant_data", "../data"
  config.vm.share_folder "omnibus-ruby", "~/omnibus-ruby", File.expand_path("../../omnibus-ruby", __FILE__)
  config.vm.share_folder "opscode-omnibus", "~/opscode-omnibus", File.expand_path("..", __FILE__)

  # Enable provisioning with chef solo, specifying a cookbooks path (relative
  # to this Vagrantfile), and adding some recipes and/or roles.
  config.vm.provision :chef_solo do |chef|
    chef.cookbooks_path = "cookbooks"
    chef.add_recipe "opscode-omnibus"
  end

  # Enable SSH agent forwarding for git clones
  config.ssh.forward_agent = true
  
  # Give enough horsepower to build PC without taking all day
  # or several hours worth of swapping  Disable support we don't need
  config.vm.customize [ 
    "modifyvm", :id,
    "--memory", "1536", 
    "--cpus", "2", 
    "--usb", "off", 
    "--usbehci", "off",
    "--audio", "none"
  ]
end
