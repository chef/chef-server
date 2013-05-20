# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  # All Vagrant configuration is done here. The most common configuration
  # options are documented and commented below. For a complete reference,
  # please see the online documentation at vagrantup.com.

  config.vm.hostname = "opscode-chef-mover-berkshelf"

  # Every Vagrant virtual environment requires a box to build off of.
  config.vm.box = "opscode-ubuntu-12.04"

  # The url from where the 'config.vm.box' box will be fetched if it
  # doesn't already exist on the user's system.
  config.vm.box_url = "https://opscode-vm.s3.amazonaws.com/vagrant/opscode_ubuntu-12.04_chef-11.4.4.box"

  # Assign this VM to a host-only network IP, allowing you to access it
  # via the IP. Host-only networks can talk to the host machine as well as
  # any other machines on the same network, but cannot be accessed (through this
  # network interface) by any external networks.
  config.vm.network :private_network, ip: "33.33.33.10"

  # Create a public network, which generally matched to bridged network.
  # Bridged networks make the machine appear as another physical device on
  # your network.

  # config.vm.network :public_network

  # Create a forwarded port mapping which allows access to a specific port
  # within the machine from a port on the host machine. In the example below,
  # accessing "localhost:8080" will access port 80 on the guest machine.

  # Share an additional folder to the guest VM. The first argument is
  # the path on the host to the actual folder. The second argument is
  # the path on the guest to mount the folder. And the optional third
  # argument is a set of non-required options.
  # config.vm.synced_folder "../data", "/vagrant_data"

  # Provider-specific configuration so you can fine-tune various
  # backing providers for Vagrant. These expose provider-specific options.
  # Example for VirtualBox:
  #
   config.vm.provider :virtualbox do |vb|
  #   # Don't boot with headless mode
  #   vb.gui = true
  #
  #   # Use VBoxManage to customize the VM. For example to change memory:
  # allow creating symlinks in /vagrant
			vb.customize ["setextradata", :id, "VBoxInternal2/SharedFoldersEnableSymlinksCreate/v-root", "1"]
			vb.customize ["modifyvm", :id, "--memory", "1024"]
   end
  #
  # View the documentation for the provider you're using for more
  # information on available options.
	config.vm.synced_folder "../moser", "/mnt/moser"
	config.vm.synced_folder "../decouch", "/mnt/decouch"

  config.ssh.max_tries = 40
  config.ssh.timeout   = 120

  # Enable SSH agent forwarding for git clones
  # and (if necessary) preprod couchdb access.
  config.ssh.forward_agent = true

  # The path to the Berksfile to use with Vagrant Berkshelf
  # config.berkshelf.berksfile_path = "./Berksfile"

  # Enabling the Berkshelf plugin. To enable this globally, add this configuration
  # option to your ~/.vagrant.d/Vagrantfile file
  config.berkshelf.enabled = true

  # An array of symbols representing groups of cookbook described in the Vagrantfile
  # to exclusively install and copy to Vagrant's shelf.
  # config.berkshelf.only = []

  # An array of symbols representing groups of cookbook described in the Vagrantfile
  # to skip installing and copying to Vagrant's shelf.
  # config.berkshelf.except = []

  # Ensure Chef is installed for provisioning
  config.omnibus.chef_version = ENV['OMNIBUS_CHEF_VERSION'] || :latest

  config.vm.provision :chef_solo do |chef|
    chef.json = {
     "postgresql" => {
        "password" => {
          "postgres" => "iloverandompasswordsbutthiswilldo"
        }
      },
      "mover" => { "dev_mode" => true,
                   "enable_demigrate" => false },
      "munin" => { "stub" => true }
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
