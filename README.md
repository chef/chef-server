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
    bundle install
    bundle exec rake db:migrate    # We're still on SQLite, here
    bundle exec rails server

If you happen to be greeted (as I was) with a Rails usage doc instead of a running server:

    $ bundle exec rails server
    Usage:
      rails new APP_PATH [options]

    Options:
      -r, [--ruby=PATH]              # Path to the Ruby binary of your choice
    .
    .
    .

...then try this ([referenced here](http://stackoverflow.com/questions/14841575/rails-4-doesnt-detect-application)):

    rake rails:update:bin     

That'll give you a running server at http://localhost:3000.  Sign in using the only (hard-coded!) credentials that work right now:

    applejack
    applejack

Well done!  Now you need an app.  Visit http://localhost:3000/oauth/applications and click **New Application**.  (This is wide open now, but we'll restrict it at some point, of course.)  Add a name and a callback URL (in the form 'http://your-hostname/some-path').  Save.

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
    bundle install
    bundle exec rake test:prepare
    bundle exec rspec

## Author

Chris Nunciato <cnunciato@getchef.com>