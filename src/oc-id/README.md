oc-id
=====

[![Build Status](https://travis-ci.org/chef/oc-id.svg)](https://travis-ci.org/chef/oc-id)

Chef identity: An [OAuth 2](http://oauth.net/2/) provider for Chef.

For an introduction to oc-id and how it works, please see this blog post [oc-id on Chef Server: An Introduction](https://www.chef.io/blog/2015/06/09/oc-id-on-chef-server-an-introduction/)

## System Requirements

Chef identity is included with Chef server ≥ 12 and Enterprise Chef server ≥
11.2. If you're running either of these you already have it running at
https://your-chef-server-url/id.

To run this app you'll need:

* Ruby >= 2.0
* Node.js (if you're compiling assets to run in production)
* The `bundler` gem
* A Postgres database instance
* A reachable Enterprise Chef 11 or Chef 12 server URL
* A privileged key for that Chef server (usually stored in
  /etc/opscode/private-chef-secrets.json on the Chef server)

## Configuration

For instructions on configuring the Chef identity that is included with Chef
server, see the
[chef-server.rb Settings documentation](https://docs.chef.io/config_rb_server_optional_settings.html#oc-id).

[RailsConfig](https://github.com/railsconfig/rails_config) is used for
application configuration. The defaults are in config/settings.yml. You can
override any key in config/settings.local.yml or config/settings/ENV.yml,
where ENV is the environment in which the application is running.

The database is configured in config/database.yml.

### Configuring Administrators

Normal users can only manage their own authorized applications. Administrators
can create and manage applications that users can authorize against.

The `doorkeeper.administrators` array in the settings is a list of usernames
(on the Chef server) that have administrative access. This is loaded when
the application starts.

### Configuring Applications

Applications that can be authorize against oc-id can be managed by
administrators by going to /id/oauth/applications.

The redirect URL is for applications using
[omniauth-chef-oauth2](https://github.com/opscode/omniauth-chef-oauth2) is
usually something like `https://host:port/auth/chef_oauth2/callback`.

## Development

You'll need all the requirements listed above. Once you have everything setup
copy the webui\_priv.pem to the config directory of oc-id. Then update the
settings.yml file, replacing the endpoint setting with the URL of the Chef Server
you'll be using for development. Alternately, you can edit the development.yml
file in the config/settings directory and ensure it has the endpoint setting
set. If development.yml has the endpoint setting set this will override what is
found in settings.yml.

You can then run the app in development mode using the usual Rails workflow:

    bundle install
    bin/rake db:create
    bin/rake db:migrate
    bin/rails server

### Assets

Historically we built assets in the omnibus pipeline, but that has
proven difficult because nodejs no longer supports all of our
platforms. Instead we are committing assets to git. This isn't ideal,
as it makes it harder alter assets in the dev-vm; they are
now synchronized from the host. 

You can locally compile assets with the command:

bin/rake assets:precompile

There are a few pain points with this approach.
* When assets change, their generated name changes because it includes a hash
* We have a lot of dead assets because we pull in common libraries

Suggested procedure for updating assets:

git rm public/id
bin/rake assets:precompile
rm -rf public/id/assets/source public/id/assets/icons/i*
git add public/id

## Test

To run the tests:

    bin/rspec

The specs in spec/requests require a running and configured Chef server.

## Production

To run in production you'll need to:

* Configure your app settings in config/settings/production.yml (don't forget the
`secret_key_base`)
* Configure your database under `production` in config/database.yml

To set everything up:

    rake assets:precompile
    RAILS_ENV=production rake db:create
    RAILS_ENV=production rake db:migrate

To run the server:

    unicorn -E production

In a real production environment you're going to want to run this behind a load
balancer with SSL, configure process monitoring, logging, etc.

A [community cookbook for Chef identity](https://supermarket.chef.io/cookbooks/oc-id)
is available on Supermarket. It is not created by or maintained by Chef
Software, but should work for running the app.

## MailCatcher

[MailCatcher](http://mailcatcher.me/) is a useful tool that can  be used to intercept and inspect email sent from the app in development mode. It is installed when running bundle install, so it ready for use. Run ```bundle exec mailcatcher``` and go to http://localhost:1080 to see messages.

## License

Chef identity

|                      |                                          |
|:---------------------|:-----------------------------------------|
| **Author:**          | Chris Nunciato <cnunciato@chef.io>
| **Copyright:**       | Copyright 2014-2018 Chef Software, Inc.
| **License:**         | Apache License, Version 2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
