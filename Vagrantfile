# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure('2') do |config|

  config.vm.box = 'opscode-ubuntu-12.04'
  config.vm.box_url = 'https://opscode-vm-bento.s3.amazonaws.com/vagrant/opscode_ubuntu-12.04_provisionerless.box'
  config.omnibus.chef_version = '11.4.4'
  config.berkshelf.enabled = true
  config.berkshelf.berksfile_path = './Berksfile'

  path_to_dev_vm = ENV['DEV_VM_PATH'] || '~/oc/opscode-dev-vm'
  path_to_cookbooks = ENV['PLATFORM_COOKBOOKS_PATH'] || '~/oc/opscode-platform-cookbooks'

  shared_settings = {
    :data_bags_path => "#{path_to_cookbooks}/data_bags",
    :roles_path => "#{path_to_cookbooks}/roles",
    :json => {}
  }

  vms = {
    :app => '33.33.33.185'
  }

  config.vm.define :app do |app|
    app.vm.synced_folder 'files/default/app', '/srv/oc-id/current/files/default/app'
    app.vm.synced_folder path_to_dev_vm, '/src/opscode-dev-vm'
    app.vm.hostname = hostname_for('app')
    app.vm.network 'private_network', :ip => vms[:app]
    app.vm.provision :chef_solo do |chef|
      chef.data_bags_path = shared_settings[:data_bags_path]
      chef.roles_path = shared_settings[:roles_path]
      chef.json = shared_settings[:json].merge({
        'oc-id' => {
          :install_source => 'local_share',
          :proxy => 'http://33.33.33.1:8888',
          :watch => true
        }
      })
      chef.run_list = [
        'recipe[opscode-dev-shim]',
        'recipe[oc-id]'
      ]
    end
  end
end

def hostname_for(name)
  "oc-id-#{name.to_s}"
end
