oc-id
=====

Chef Identity: An [OAuth 2](http://oauth.net/2/) provider for Chef.

## Getting Started

I'm still ironing out the Vagrant and Test Kitchen workflows here, but if you're inside the walls of Chef and happen to have repos at these two paths:

    ~/oc/opscode-platform-cookbooks/
    ~/oc/opscode-dev-vm/

... then you should be able to both ``vagrant up`` and ``kitchen test`` this stuff.

If you're outside of said walls, provided you're running Rails 4, you should be able to start the app by:

    cd files/default/app
    bundle install --binstubs
    bin/rake db:migrate    # We're still on SQLite, here
    bin/rails server 

That'll give you a running server at http://localhost:3000.  Sign in using a valid Chef server account:

    applejack
    applejack

(By default, we point to dev-vm at [https://api.opscode.piab](https://api.opscode.piab); you can customize your configuration settings in config/settings/*.yml as needed.  See below for instructions on integrating with Enterprise Chef.)

Well done!  Now you need an app.  Visit [http://localhost:3000/oauth/applications](http://localhost:3000/oauth/applications) and click **New Application**.  (This is wide open now, but we'll restrict it at some point, of course.)  Add a name and a callback URL (in the form 'http://your-hostname/some-path').  Save.

Now it's time to authorize your app!  As a shortcut, click that **Authorize** link.  It'll open a new tab.  Then **note the value** of the ``code`` parameter and open a Terminal tab; we're going to use the [oauth2 gem](https://github.com/intridea/oauth2) to simulate a three-legged OAuth flow.  To do that:

    $ gem install oauth2
    $ irb -r oauth2

Then in IRB:

    callback = YOUR_CALLBACK_URL
    app_id = YOUR_APP_ID
    app_secret = YOUR_APP_SECRET
    client = OAuth2::Client.new(app_id, app_secret, :site => 'http://localhost:3000/')
    access = client.auth_code.get_token(THE_CODE_YOU_GOT_ABOVE, :redirect_uri => callback)
    "curl -v -H 'Authorization: Bearer #{access.token}' -H 'Accept: application/json' 'http://localhost:3000/users/applejack'"

Then take that ``curl`` command and paste it into another Terminal tab.  You should see some lovely JSON.

Lots more work to be done here, but it's a start.

## Running the Tests

    cd files/default/app
    bundle install --binstubs
    bin/rake test:prepare
    bin/rspec

## Using oc-id with Enterprise Chef

To authenticate with oc-id against a Vagrant-managed Enterprise Chef instance...

  1. Set up a new Vagrant box.  Heres's a minimal Vagrantfile that should suffice:

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

  1. Copy the ``webui_priv.pem`` key from /etc/opscode/webui_priv.pem to your local (host) machine, then exit:

        $ cp /etc/opscode/webui_priv.pem /vagrant
        $ exit

  1. Copy the key into the Rails project &mdash; e.g.:

        $ cp webui_priv.pem ../oc-id/files/default/app/config/

  1. Open management console, which should now be available at https://33.33.33.11, as configured in our sample Vagrantfile, and click **Sign Up** to create a new user.  (I use applejack/applejack here as well, since it's what the tests will be looking for.)

  1. Back in your Rails root, you should now be able to run the tests (assuming you ran ``test:prepare`` as described in the previous section):

        bundle exec rspec

You should now be able to sign in with oc-id with your newly created Enterprise Chef user as well.

## Author

Chris Nunciato <cnunciato@getchef.com>