ANALYTICS_VM_ADDRESS="192.168.33.160"
require "yaml"

def load_analytics_attributes
    attributes = YAML.load_file("analytics_defaults.yml")
    begin
        custom_attributes = YAML.load_file("analytics_config.yml")
        attributes = simple_deep_merge(attributes, custom_attributes)
    rescue
    end
    attributes
end


def merge_analytics_settings(cs_atts)
    attributes = YAML.load_file("cs_analytics_defaults.yml")
    atts = simple_deep_merge(cs_atts, attributes)
    atts
end

def add_analytics_provisioning_atts(attributes)
    # I suppose this setting could be in a yaml file
    # but the thought of a single entry in the file seems so lonely
    attributes['vm']['node-attributes'] = {}
    attributes['vm']['node-attributes']['provisioning'] = {}
    attributes['vm']['node-attributes']['provisioning']['chef-server-config'] = {}
    attributes['vm']['node-attributes']['provisioning']['chef-server-config']['analytics'] = true
end

def define_analytics_server(config)
  attributes = load_analytics_attributes

  action = ARGV[0]
  provisioning, installer, installer_path =
        PackagePrompt.new("opscode-analytics", "Chef Analytics", "ANALYTICS_INSTALLER", "ANALYTICS_AUTOPACKAGE").prepare(action)
  config.vm.hostname = "analytics.dev"
  config.vm.network "private_network", ip: ANALYTICS_VM_ADDRESS

  config.vm.provider :virtualbox do |vb|
    vb.customize ["modifyvm", :id,
                  "--name", "chef-analytics",
                  "--memory", attributes["vm"]["memory"],
                  "--cpus", attributes["vm"]["cpus"],
                  "--usb", "off",
                  "--usbehci", "off"
    ]
  end
  if provisioning
    json = {
      "packages" => attributes["vm"]["packages"],
      "tz" => host_timezone,
      "omnibus-autoload" => attributes["vm"]["omnibus-autoload"]
    }.merge attributes["vm"]["node-attributes"]

    dotfiles_path = attributes["vm"]["dotfile_path"] || "dotfiles"
    config.vm.synced_folder File.absolute_path(File.join(Dir.pwd, "../")), "/host",
      type: "rsync",
      rsync__args: ["--verbose", "--archive", "--delete", "-z", "--no-owner", "--no-group" ],
      rsync__exclude: attributes["vm"]['sync']['exclude-files']
    # We're also going to do a share of the slower vboxsf style, allowing us to auto-checkout dependencies
    # and have them be properly synced to a place that we can use them.
    config.vm.synced_folder installer_path, "/installers"
    config.vm.synced_folder File.expand_path(dotfiles_path), "/dotfiles"

    config.vm.provision "file", source: "~/.gitconfig", destination: ".gitconfig"
    config.vm.provision "shell", inline: install_hack(installer)
    config.vm.provision "chef_solo" do |chef|
      chef.install = false
      chef.binary_path = "/opt/opscode-analytics/embedded/bin"
      chef.node_name = config.vm.hostname
      chef.cookbooks_path = "cookbooks"
      chef.add_recipe("provisioning::chef-analytics")
      #chef.add_recipe("dev::system") TODO
      #chef.add_recipe("dev::user-env") TODO
      #chef.add_recipe("dev::dvm") TODO
      chef.json = json || {}
    end
    # once the chef-server VM is provisioned, copy over actions-source.json so we
    # know where to point the Alaska Pipeline in Analytics
    config.vm.provision "shell", inline: "cp /installers/actions-source.json /etc/opscode-analytics/actions-source.json"
    # Makes more sense here than in a one-off line in the dvm recipe, which
    # has no direct connection...
    config.vm.provision "shell", inline: "opscode-analytics-ctl reconfigure"
  end
end


