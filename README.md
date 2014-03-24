oc-id
=====

Chef Identity: An [OAuth 2](http://oauth.net/2/) provider for Chef.

## Getting Started

See below for instructions for Enterprise Chef (or dev-vm); you'll need a Chef server endpoint, an associated **webui** key at ``config/webui_priv.pem``, and the appropriate configuration overrides at ``config/settings.local.yml``.

Once that's set up, you should be able to run the app in development mode using the usual Rails workflow:

    $ bundle install
    $ bin/rake db:migrate
    $ bin/rails server

Verified with:

  * Berkshelf (2.0.14)
  * Vagrant 1.4.3
  * Test Kitchen version 1.2.1

## Running the Tests

### Application Tests

    $ bin/rake db:test:prepare
    $ bin/rspec

### Cookbook Tests

    $ cd chef/cookbooks/oc-id
    $ bundle install
    $ kitchen test

## Using oc-id with Enterprise Chef

To authenticate with oc-id against a Vagrant-managed Enterprise Chef instance:

  1. Set up a Vagrant box for Enterprise Chef.  Heres's a minimal Vagrantfile that should suffice:

        # -*- mode: ruby -*-
        # vi: set ft=ruby :

        Vagrant.configure("2") do |config|
          config.vm.box = 'opscode-ubuntu-12.04'
          config.vm.box_url = 'https://opscode-vm-bento.s3.amazonaws.com/vagrant/opscode_ubuntu-12.04_provisionerless.box'
          config.vm.hostname = 'ec-11'
          config.vm.network 'private_network', :ip => '33.33.33.11'
          config.vm.provider 'virtualbox' do |v|
            v.memory = 4096
            v.cpus = 4
          end
        end

  1. Inside the vm, download, install and configure Enterprise Chef:

        $ wget http://path.to/private-chef_11.1.2-1.ubuntu.12.04_amd64.deb
        $ sudo sudo dpkg -i private-chef_11.1.2-1.ubuntu.12.04_amd64.deb
        $ sudo private-chef-ctl reconfigure

  1. Still inside then VM, copy the ``webui_priv.pem`` key from /etc/opscode/webui_priv.pem to your local (host) machine, then exit:

        $ cp /etc/opscode/webui_priv.pem /vagrant
        $ exit

  1. You should now see ``webui_priv.pem`` alongside your Vagrantfile.  Now move that file into to your oc-id project, where the Rails app can read it:

        $ mv ./webui_priv.pem {your-oc-id-project-root}/config/

  1. Open management console (which should now be available at https://33.33.33.11, as configured in our sample Vagrantfile) and click **Sign Up** to create a new user.  (I use **applejack**/**applejack** here as well, since it's what the tests will be looking for.)

  1. And now in your oc-id (Rails) root, you should be able to run the tests:

        $ bin/rake db:test:prepare
        $ bin/rspec

You should now be able to sign in with oc-id with your newly created Enterprise Chef user as well.

## Author

Chris Nunciato <cnunciato@getchef.com>
