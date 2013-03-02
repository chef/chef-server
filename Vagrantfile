require 'berkshelf/vagrant'

OMNIBUS_CHEF_VERSION = "11.4.0"

Vagrant::Config.run do |config|
  # All Vagrant configuration is done here. The most common configuration
  # options are documented and commented below. For a complete reference,
  # please see the online documentation at vagrantup.com.

  # The path to the Berksfile to use with Vagrant Berkshelf
  # config.berkshelf.berksfile_path = "./Berksfile"

  # An array of symbols representing groups of cookbook described in the Vagrantfile
  # to skip installing and copying to Vagrant's shelf.
  # config.berkshelf.only = []

  # An array of symbols representing groups of cookbook described in the Vagrantfile
  # to skip installing and copying to Vagrant's shelf.
  # config.berkshelf.except = []

  config.vm.host_name = "opscode-chef-mover-berkshelf"

  #config.vm.box = "opscode-ubuntu-10.04"
  #config.vm.box_url = "http://opscode-vm.s3.amazonaws.com/vagrant/opscode_ubuntu-10.04_chef-10.18.2.box"
  config.vm.box = "opscode-ubuntu-12.04"
  config.vm.box_url = "https://opscode-vm.s3.amazonaws.com/vagrant/boxes/opscode-ubuntu-12.04.box"

  # Boot with a GUI so you can see the screen. (Default is headless)
  # config.vm.boot_mode = :gui

  # Assign this VM to a host-only network IP, allowing you to access it
  # via the IP. Host-only networks can talk to the host machine as well as
  # any other machines on the same network, but cannot be accessed (through this
  # network interface) by any external networks.
  config.vm.network :hostonly, "33.33.33.10"

  # Assign this VM to a bridged network, allowing you to connect directly to a
  # network using the host's network device. This makes the VM appear as another
  # physical device on your network.

  # config.vm.network :bridged

  # Forward a port from the guest to the host, which allows for outside
  # computers to access the VM, whereas host only networking does not.
  # config.vm.forward_port 80, 8080

  # Share an additional folder to the guest VM. The first argument is
  # an identifier, the second is the path on the guest to mount the
  # folder, and the third is the path on the host to the actual folder.
  # config.vm.share_folder "v-data", "/vagrant_data", "../data"

  config.ssh.max_tries = 40
  config.ssh.timeout   = 120
  # Enable SSH agent forwarding for git clones
  config.ssh.forward_agent = true

  # allow creating symlinks in /vagrant
  config.vm.customize ["setextradata", :id, "VBoxInternal2/SharedFoldersEnableSymlinksCreate/v-root", "1"]

  config.vm.provision :shell, :inline => <<-INSTALL_OMNIBUS
  if [ ! -d "/opt/chef" ] ||
     [ ! $(chef-solo --v | awk "{print \\$2}") = "#{OMNIBUS_CHEF_VERSION}" ]
  then
    wget -qO- https://www.opscode.com/chef/install.sh | sudo bash -s -- -v #{OMNIBUS_CHEF_VERSION}
  else
    echo "Chef #{OMNIBUS_CHEF_VERSION} already installed...skipping installation."
  fi
  INSTALL_OMNIBUS

  if ENV['OPSCODE_PLATFORM_REPO'].nil?
    puts "ERROR: please export OPSCODE_PLATFORM_REPO"
    exit 1
  end
  config.vm.provision :chef_solo do |chef|
    chef.json = {
     "postgresql" => {
        "password" => {
          "postgres" => "iloverandompasswordsbutthiswilldo"
        }
      },
      "mover" => { "dev_mode" => true }
    }
    chef.data_bags_path = "#{ENV['OPSCODE_PLATFORM_REPO']}/data_bags"
    chef.roles_path = "#{ENV['OPSCODE_PLATFORM_REPO']}/roles"
    chef.run_list = [
      "recipe[opscode-dev-shim]",
      "recipe[opscode-chef-mover::dev]",
      "recipe[opscode-chef-mover::default]"
    ]
  end
end
