# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  config.vm.hostname = "chef-mover"
  #config.vm.box = "raring64"
  config.vm.box = "opscode-ubuntu-12.04"
  config.vm.box_url = "https://opscode-vm.s3.amazonaws.com/vagrant/opscode_ubuntu-12.04_chef-11.4.4.box"
  config.vm.network :private_network, ip: "33.33.33.10"
  config.vm.provider :virtualbox do |vb|
			vb.customize ["setextradata", :id, "VBoxInternal2/SharedFoldersEnableSymlinksCreate/v-root", "1"]
			vb.customize ["modifyvm", :id, "--memory", ENV['MOVER_RAM'] || "1024"]
			vb.customize ["modifyvm", :id, "--cpus", ENV['MOVER_CPU_COUNT'] || "4"]
   end

  config.vm.synced_folder "../moser", "/mnt/moser"
	config.vm.synced_folder "../decouch", "/mnt/decouch"

  config.ssh.max_tries = 40
  config.ssh.timeout   = 120
  config.ssh.forward_agent = true
  config.berkshelf.enabled = true
  config.omnibus.chef_version = ENV['OMNIBUS_CHEF_VERSION'] || :latest

  config.vm.provision :chef_solo do |chef|
    chef.json = {
     "postgresql" => {
        "password" => {
          "postgres" => "iloverandompasswordsbutthiswilldo"
        }
      },
      "mover" => { "dev_mode" => true,
                   "enable_demigrate" => false,
                   "schema_rev" => "master" },
      "munin" => { "stub" => true },
      "sqitch" => { "engine" => "pg" }
    }
    chef.data_bags_path = "#{ENV['OPSCODE_PLATFORM_REPO']}/data_bags"
    chef.roles_path = "#{ENV['OPSCODE_PLATFORM_REPO']}/roles"
    chef.run_list = [
      "recipe[opscode-dev-shim]",
      "recipe[opscode-chef-mover::dev]",
      "recipe[opscode-chef-mover::default]",
      "recipe[opscode-xdarklaunch::dev]",
      "recipe[opscode-xdarklaunch::default]"
    ]
  end
end
