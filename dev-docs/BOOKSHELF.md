## Bookshelf

### What it Does
Coming soon

### High Level Flow
[High Level Flow](bookshelf-sigv4-flow.txt)

### How it works
Coming soon

### Using S3 instead of Bookshelf

If you would like to configure chef-server to use S3 instead of Bookshelf, you can
type the following into the bash shell of your running chef-server.  This assumes
you are using a root login.  Make obvious substitutions as needed, e.g. s3_bucket,
access_key_id, etc:

```
# copy these configs into the bottom of /etc/opscode/chef-server.rb
echo "
bookshelf['enable'] =               false
bookshelf['vip'] =                  's3.us-east-2.amazonaws.com'         # alter to taste
bookshelf['external_url'] =         'https://s3.us-east-2.amazonaws.com' # alter to taste
opscode_erchef['s3_bucket'] =       'YOUR-BUCKET-HERE'
bookshelf['access_key_id'] =        'YOUR-ID-HERE'
bookshelf['secret_access_key'] =    'YOUR-SECRET-HERE'">>/etc/opscode/chef-server.rb

# put these values into /etc/environment
echo '
AWS_ACCESS_KEY_ID="YOUR-ID-HERE"
AWS_SECRET_ACCESS_KEY="YOUR-SECRET-HERE"'>>/etc/environment

# uptake new configs and settings
chef-server-ctl set-secret bookshelf access_key_id YOUR-ID-HERE
chef-server-ctl set-secret bookshelf secret_access_key YOUR-SECRET-HERE
exit
sudo -i
chef-server-ctl reconfigure
chef-server-ctl stop opscode-erchef
chef-server-ctl start opscode-erchef
```
