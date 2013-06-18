# -*- mode: ruby -*-

BIFROST_DB_HOST         = "33.33.33.20"
BIFROST_DB_PORT         =  5432 # This is actually fixed in the recipe currently
BIFROST_FORWARD_DB_PORT = 15432

BIFROST_API_HOST        = "33.33.33.21"
BIFROST_PORT            = 5959
BIFROST_FORWARD_PORT    = 15959

METRICS_HOST             = "33.33.33.22"
METRICS_ESTATSD_PORT     = 5665

BOX_ID = "opscode-ubuntu-10.04"
BOX_URL = "https://opscode-vm.s3.amazonaws.com/vagrant/boxes/opscode-ubuntu-10.04.box"

Vagrant.configure("2") do |config|

  # Current version of Chef in prod... update as needed.
  config.omnibus.chef_version = '11.4.4'

  config.berkshelf.enabled = true

  config.vm.box = BOX_ID
  config.vm.box_url = BOX_URL

  config.vm.provider :virtualbox do |vb|
    vb.customize ["modifyvm", :id, "--nictype1", "virtio"] # NAT NIC
    # Use the host's resolver for DNS queries
    vb.customize ["modifyvm", :id, "--natdnshostresolver1", "on"]
  end

  config.ssh.max_tries = 40
  config.ssh.timeout   = 120
  config.ssh.forward_agent = true

  config.vm.define :db do |db_config|
    db_config.vm.hostname = "oc-bifrost-db-berkshelf"

    db_config.vm.network :private_network, :ip => BIFROST_DB_HOST, :adapter => 2

    db_config.vm.provider :virtualbox do |vb|
      vb.customize ["modifyvm", :id, "--nictype2", "virtio"] # host-only NIC
    end

    db_config.vm.network :forwarded_port, :guest => BIFROST_DB_PORT,:host => BIFROST_FORWARD_DB_PORT

    db_config.vm.provision :chef_solo do |chef|
      chef.json = {
        "oc_bifrost" => {
          "development_mode" => true
        },
        # When running in a chef-solo setting, the postgres user
        # password must be defined here, since there is no Chef Server
        # to persist it to.
        "postgresql" => {
          "password" => {
            "postgres" => "honeybadger"
          },
          "config" => {
            "port" => BIFROST_DB_PORT
          }
        }
      }

      chef.roles_path = "#{ENV['OPSCODE_PLATFORM_REPO']}/roles"
      chef.data_bags_path = "#{ENV['OPSCODE_PLATFORM_REPO']}/data_bags"

      chef.run_list = ["recipe[opscode-dev-shim]",
                       "recipe[opscode-bifrost::database]"]
    end
  end

  config.vm.define :api do |api_config|
    api_config.vm.hostname = "oc-bifrost-api-berkshelf"

    api_config.vm.network :private_network, :ip => BIFROST_API_HOST, :adapter => 2

    api_config.vm.provider :virtualbox do |vb|
      vb.customize ["modifyvm", :id, "--nictype2", "virtio"] # host-only NIC
    end

    api_config.vm.network :forwarded_port, :guest => BIFROST_PORT, :host => BIFROST_FORWARD_PORT

    api_config.vm.provision :chef_solo do |chef|
      chef.json = {
        "oc_bifrost" => {
          "development_mode" => true,
          "database" => {
            "host" => BIFROST_DB_HOST,
            "port" => BIFROST_DB_PORT
          }
        },
        # These values are hard-coded into the PIAB monitoring cookbooks
        # we're currently using for graphite in dev... we don't need a
        # running estatsd server for this to work, though.
        "stats_hero" => {
          "estatsd_host" => METRICS_HOST,
          "estatsd_port" => METRICS_ESTATSD_PORT
        }
      }

      chef.roles_path = "#{ENV['OPSCODE_PLATFORM_REPO']}/roles"
      chef.data_bags_path = "#{ENV['OPSCODE_PLATFORM_REPO']}/data_bags"

      chef.run_list = [
                       "recipe[opscode-dev-shim]",
                       "recipe[opscode-bifrost::api_server]"
                      ]
    end
  end

  config.vm.define :metrics do |metrics_config|
    metrics_config.vm.hostname = "oc-bifrost-metrics-berkshelf"

    metrics_config.vm.network :private_network, :ip => METRICS_HOST, :adapter => 2

    metrics_config.vm.provider :virtualbox do |vb|
      vb.customize ["modifyvm", :id, "--nictype2", "virtio"] # host-only NIC
    end

    # The dev-vm cookbooks assume an Omnibus directory structure; this shell provisioner fakes it
    metrics_config.vm.provision :shell, :inline => "mkdir -p /opt/opscode"
    metrics_config.vm.provision :chef_solo do |chef|
      chef.roles_path = "#{ENV['OPSCODE_PLATFORM_REPO']}/roles"
      chef.data_bags_path = "#{ENV['OPSCODE_PLATFORM_REPO']}/data_bags"
      chef.json = {
        "gdash" => {
          "graphite_url" => "http://#{METRICS_HOST}:8080",
          "interface" => "0.0.0.0"
        }
      }
      chef.run_list = ["recipe[opscode-dev-shim]",
                       "recipe[git]",
                       "recipe[erlang_binary]",
                       "recipe[erlang_binary::rebar]",
                       "recipe[piab::monitoring]",
                       "opscode-ruby::default",
                       "gdash::default",
                       "opscode-bifrost::gdash"]
    end
  end
end
