oc-id
=====

Chef Identity: An [OAuth 2](http://oauth.net/2/) provider for Chef.

## Getting Started

See below for instructions for Enterprise Chef (or dev-vm); you'll need a Chef server endpoint, an associated **webui** key at ``config/webui_priv.pem``, and the appropriate configuration overrides at ``config/settings.local.yml``.

Once that's set up, you should be able to run the app in development mode using the usual Rails workflow:

    $ bundle install
    $ bin/rake db:migrate
    $ bin/rails server

Verified with Berkshelf 2.0.14, Vagrant 1.4.3 and Test Kitchen version 1.2.1.

## Tests

### Application Tests

These assume the presence of a Chef server at 33.33.33.10 (configured at config/settings.yml).

    $ bin/rake db:test:prepare
    $ bin/rspec

### Cookbook Tests

See the oc-id cookbook (at chef/cookbooks/oc-id) and the Kitchen configuration file (chef/cookbooks/oc-id/.kitchen.yml) for details.  

    $ cd chef/cookbooks/oc-id
    $ bundle install
    $ kitchen converge
    $ kitchen verify

## Using oc-id with Enterprise Chef

To work on the oc-id app, you'll need a running Chef server; either dev-vm or a clean EC install should be fine, as long as the Rails app finds a ``webui_priv.pem`` key at ./config.  Here are some instructions for setting up anew:

  1. A Vagrantfile for privisioning an EC install is included with this project.  To use it, first get an EC download URL (e.g., [from the support site](http://support.opscode.us/releases)), then:

        $ cd chef/cookbooks/oc-id
        $ EC_URL="http://path.to.a/private-chef...deb" vagrant up ec

  1. When provisioning completes, you should see a ``webui_priv.pem`` key alongside the Vagrantfile.  Move that file into to the oc-id Rails project (a convention is to put it into ``#{Rails.root}/config)``) so the Rails app can read it:

        $ mv ./webui_priv.pem ../../../config/

  1. Open management console (which should now be available at [https://33.33.33.10](https://33.33.33.10)) and click **Sign Up** to create a new user.  (I use **applejack**/**applejack** here as well, since it's what the tests will be looking for.)

  1. And now in your oc-id (Rails) root, you should be able to run the tests:

        $ bin/rake db:test:prepare
        $ bin/rspec

  1. ... or just run the Rails app:

        $ bin/rails server

You should now be able to sign into oc-id with your newly created Chef user as well.

## Author

Chris Nunciato <cnunciato@getchef.com>
