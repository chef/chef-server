require 'chef'
require 'sequel'
require 'highline/import'


# General comments:
#
# How can these actions be made idempotent as best as possible?
#
# A way should be provided to either suppress all output, or else
# have it suppressed by default and then made available if a verbose
# flag is set (what do the other commands do by default? Can their
# output be tuned to this level?)

def run_osc_upgrade

  # Main function
  def run_upgrade
    # This is to help make this process idempotent, but currently if EC has
    # been started at all OSC won't come up properly. ctl will report EC as
    # stopped and OSC up, but any attempt to access OSC will result in a 502
    # This issue will have to be solved for this process to be idempotent
    stop_ec

    start_osc

    log "Preparing knife to download data from the Open Source Chef server"

    # As a precaution, we want the location to vary. TODO: We need to save
    # the value to a read protected file so it can be read from if a resume
    # is needed
    # Do we want to delete the directory on failure or leave it for debugging?
    # Are the permissions good enough? (0700)
    osc_data_dir = Dir.mktmpdir('osc-chef-server-data')
    log "Making #{osc_data_dir} as the location to save the server data"

    write_knife_config(osc_data_dir)

    log "Downloading data from the Open Source Chef server"

    run_knife_download

    key_file = "#{osc_data_dir}/key_dump.json"
    create_osc_key_file(key_file)

    log "Finished downloading data from the Open Source Chef server"

    stop_osc

    log "Configuring the Enterprise Chef server for use"

    reconfigure(false)

    start_ec

    log "Transforming Open Source server data for upload to Enterprise Chef server"

    # To prepare the downloaded OSC data for upload to the EC server
    # it is put into a file structure that knife-ec-backup expects
    # and then knife-ec-backup restore functionality is used to upload it to the
    # new Chef server.

    org_name, org_full_name, org_type = determine_org_name

    # See note above on osc_data_dir
    ec_data_dir = Dir.mktmpdir('ec-chef-server-data')
    org_dir = "#{ec_data_dir}/organizations/#{org_name}"
    make_dir("#{ec_data_dir}/organizations", 0644)
    org_dir = "#{ec_data_dir}/organizations/#{org_name}"
    make_dir(org_dir, 0644)

    create_org_json(org_dir, org_name, org_full_name, org_type)

    # Copy over the key_dump.json file
    FileUtils.cp(key_file, "#{ec_data_dir}/key_dump.json")

    # Copy over users
    FileUtils.cp_r("#{osc_data_dir}/users", "#{ec_data_dir}/users")

    # Copy over clients, cookbooks, data_bags, environments, nodes, roles
    %w{clients cookbooks data_bags environments nodes roles}.each do |name|
      FileUtils.cp_r("#{osc_data_dir}/#{name}", "#{org_dir}/#{name}")
    end

    user_names, admin_users = transform_osc_user_data(osc_data_dir, ec_data_dir)

    create_invitations_json(org_dir)

    create_members_json(user_names, org_dir)

    groups_dir = "#{org_dir}/groups"
    make_dir(groups_dir, 0644)

    # Under organizations/#{org_name}/groups, an admins.json and billing-admins.json is needed.
    # Any admins from OSC need to go into the admins group, as that is how it is determined that
    # a user is an admin in EC
    # All admins will be billing admins due to lack of a better selection criteria

    create_admins_json(admin_users, groups_dir)
    create_billing_admins(admin_users, groups_dir)

    write_knife_ec_backup_config

    log "Uploading transformed Open Source server data to Enterprise Chef server"

    run_knife_ec_restore(ec_data_dir)

    log "Open Source chef server upgraded to an Enterprise Chef server"

    # Need to ensure all of OSC is stopped
    # By default, pkill sends TERM, which will cause runsv,runsvdir, and svlogd to shutdown
    # Doing this will completely hose OSC so that a start command won't restart it, if needed
    # If we're going to make this process as idempotent as possible, we'll need to work around this
    # Maybe it's best to do this at the end ... or do we let the user manually remove OSC
    # with a package uninstall?
    # Once we do this we lose most hope at being idempotent
    # Maybe prompt the user before doing this and off to give them steps to do this
    # manually if they don't want this done automatically?
  #  log "Ensuring all the runit processes associated with the Open Source Server are stopped"
  #  run_command("pkill runsv")
  #  run_command("pkill runsvdir")
  #  run_command("pkill svlogd")
    # epmd lives outside of runit, but a pkill will do just fine here too
  #  run_command("pkill epmd")

    # This could all be run at the end, so that OSC could be brought back up if
    # needed, but then this would also kill EC. This code will need to be re-worked
    # so it only removes OSC and leaves EC in place, or else the user needs to
    # be directed on how to do this.

    # The OSC bits still live on the system - do we delete them here?
    # For example, /opt/chef-server is still in the path, but /opt/opscode is not
    # on dev-vm testing
    # This has the effect of making the default knife, gem, etc the chef-server versions

    # The migration data still lives on the system - it is probably worth while
    # to include an optional step to delete it, if the user specifies this, other
    # wise leave it on the system
  end


  def start_osc
    # Assumption is EC isn't running, since we detected OSC on the system
    log 'Ensuring the Open Source Chef server is started'
    msg = "Unable to start Open Source Chef server, which is needed to complete the upgrade"
    check_status(run_command("chef-server-ctl start"), msg)
    # start command can return faster than the services are ready; resulting in 502 gateway
    sleep(10)
  end

  def check_status(status, msg)
    unless status.success?
      log msg
      exit 1
    end
  end

  def write_knife_config(osc_data_dir)
    # Hard coded path to key (stole idea to use from pedant), but the path is in attributes
    # Need to ensure we have a valid path to the key here

    # Do the rest of these need to be arguments?
    config = <<-EOH
      chef_server_url "#{@options.chef_server_url}"
      node_name 'admin'
      client_key '/etc/chef-server/admin.pem'
      repo_mode 'everything'
      versioned_cookbooks true
      chef_repo_path "#{osc_data_dir}"
    EOH

    log "Writing knife config to /tmp/knife-config.rb for use in downloading the data"
    # An exception will be raised if this fails. Catch it and try to die gracefully?
    File.open("/tmp/knife-config.rb", "w"){ |file| file.write(config)}
  end

  def run_knife_download
    log "Running knife download"
    cmd = "/opt/chef-server/embedded/bin/knife download -c /tmp/knife-config.rb /"
    status = run_command(cmd)
    check_status(status, "knife download failed with #{status}")
  end

  def make_dir(dir, permissions)
    Dir.mkdir(dir, permissions) unless File.directory?(dir)
  end

  # TODO(jmink) Add error handling
  def pull_osc_db_credentials
    # This code pulled from knife-ec-backup and adapted
    log "Pulling needed db credintials"
    if !File.exists?("/etc/chef-server/chef-server-running.json")
      log "Failed to find /etc/chef-server/chef-server-running.json"
      exit 1
    end

    running_config = JSON.parse(File.read("/etc/chef-server/chef-server-running.json"))
    sql_host = running_config['chef_server']['postgresql']['vip']
    sql_port = running_config['chef_server']['postgresql']['port']
    sql_user = running_config['chef_server']['postgresql']['sql_user']
    sql_password = running_config['chef_server']['postgresql']['sql_password']
    [sql_host, sql_port, sql_user, sql_password]
  end

  def create_osc_key_file(key_file)
    sql_host, sql_port, sql_user, sql_password = pull_osc_db_credentials

    server_string = "#{sql_user}:#{sql_password}@#{sql_host}:#{sql_port}/opscode_chef"
    db = ::Sequel.connect("postgres://#{server_string}")

    # OSC doesn't have pubkey_version - what should this be filled in as?
    # EC uses an enum for hash_type that OSC doesn't use.
    # Need to determine the appropriate values and adjust the data accordingly.
    sql_user_data = db.select(:username, :id, :public_key, :hashed_password, :salt, :hash_type).from(:osc_users)
    sql_users_json =  sql_user_data.all.to_json
    sql_users = JSON.parse(sql_users_json)
    sql_users.each do |user|
      # Assuming bcrypt for now, but there are very likely incompatibilities and edge cases here
      # This needs more work
      user["hash_type"] = 'bcrypt'
      # Set pubkey_version as 0 and not 1 (1 means we a cert was used - it needs to be verified
      # this was never an option in OSC)
      user["pubkey_version"] = 0
    end
    File.open(key_file, 'w') { |file| file.write(Chef::JSONCompat.to_json_pretty(sql_users))}
  end

  def start_ec
    log "Ensuring all the Enterprise Chef server components are started"
    msg = "Unable to start Enterprise Chef, which is needed to complete the upgrade"
    status = run_command("private-chef-ctl start")
    check_status(status, msg)
    # Wait for services to come up.
    # This is longer than for OSC because we need the org system to be provisioned
    # It might be more ideal to rely on the service retry logic (just take this sleep out)
    # or to build in some kind of retry logic here.
    # In testing, the commands begin working on the 4th retry of 5, but that might not
    # be true on slower systems
    # TODO: Replace this with logic that pings the _status endpoint to see if
    # the services are up, or retries an operation until it succeeds
    sleep(120)
  end

  def stop_ec
    log 'Ensuring the Enterprise Chef server is stopped'
    msg = "Unable to stop the Enterprise Chef server, which is needed to complete the upgrade"
    status = run_command("private-chef-ctl stop")
    check_status(status, msg)
  end

  def stop_osc
    log 'Ensuring the Open Source Chef server is stopped'
    msg = "Unable to stop Open Source Chef server, which is needed to complete the upgrade"
    status = run_command("chef-server-ctl stop")
    check_status(status, msg)
  end

  def determine_org_name
    org_name = @options.org_name || ask("Chef Organization Name? ")
    org_full_name = @options.full_org_name || ask("The full Chef Organization Name? ")
    org_type = 'Business'

    [org_name, org_full_name, org_type]
  end

  def create_org_json(org_dir, org_name, org_full_name, org_type)
    # How is the private key returned to the user creating the org in this way?
    org_json = {"name" => org_name, "full_name" => org_full_name, "org_type" => org_type}
    # An exception will be raised if this fails. Catch it and try to die gracefully?
    File.open("#{org_dir}/org.json", "w"){ |file| file.write(Chef::JSONCompat.to_json_pretty(org_json)) }
  end

  def transform_osc_user_data(osc_data_dir, ec_data_dir)
    # User transform needed: add display_name, email; change name to user name
    # Need to inform the user they will have to change some of this information,
    # Or give the user a tool that they can go in and update after the update
    # information like email addresses?
    # Password resets won't work until a valid email is put into place
    user_names = []
    admin_user_names = []
    Dir.glob("#{osc_data_dir}/users/*") do |file|
      # Do a try catch for each file.  Write users that failed to a errored user file?
      # If any failed should we stop the upgrade process?
      user = Chef::JSONCompat.from_json(IO.read(file), :create_additions => false)
      user_names << user['name']
      admin_user_names << user['name'] if user['admin']
      user.delete('admin')

      # EC chef expects username not name
      user['username'] = user['name']
      user['display_name'] = user['username']
      user.delete('name')

      # Take in a default email domain?
      user['email'] = "#{user['username']}@example.com"
      File.open("#{ec_data_dir}/users/#{File.basename(file)}", "w"){ |new_file| new_file.write(Chef::JSONCompat.to_json_pretty(user)) }
    end
    [user_names, admin_user_names]
  end

  def create_invitations_json(org_dir)
    # OSC doesn't have the concept of invitations, so this is empty
    # An exception will be raised if this fails. Catch it and try to die gracefully?
    File.open("#{org_dir}/invitations.json", "w"){ |file| file.write([])}
  end

  def create_members_json(users, org_dir)
    # Members.json is under organizations/#{org_name}.
    # It looks like this:
    #
    # [
    #  {
    #   "user": { "username": "users's username"}
    #  },
    #  {
    #   "user": { "username": "users's username"}
    #  }
    # ]
    #
    members_json = []
    users.each do |name|
      user_json= {"user" => {"username" => name}}
        members_json << user_json
      end

      File.open("#{org_dir}/members.json", "w"){ |file| file.write(Chef::JSONCompat.to_json_pretty(members_json)) }
  end

  def create_admins_json(users, groups_dir)
    admins_json = { "name" => "admins", "users" => users }
    File.open("#{groups_dir}/admins.json", "w"){ |file| file.write(Chef::JSONCompat.to_json_pretty(admins_json)) }
  end

  def create_billing_admins(users, groups_dir)
    billing_admins_json = { "name" => "billing-admins", "users" => users }
    File.open("#{groups_dir}/billing-admins.json", "w"){ |file| file.write(Chef::JSONCompat.to_json_pretty(billing_admins_json)) }
  end

  def write_knife_ec_backup_config
    # These are hard coded values that need to ensure are correct
    # or else give the user a way to specify them
    # The server root is likely the same as was set for the knife config
    # used by knife download
    config = <<-EOH
    chef_server_root '#{@options.chef_server_url}'
    node_name 'pivotal'
    client_key '/etc/opscode/pivotal.pem'
    EOH

    log "Writing knife-ec-backup config to /tmp/knife-ec-backup-config.rb"
    File.open("/tmp/knife-ec-backup-config.rb", "w"){ |file| file.write(config)}
  end

  def run_knife_ec_restore(ec_data_dir)
    # --skip-useracl skip importing user acls, which will just give the user's default acls. This is the
    # desired state anyway
    # --with-user-sql pull data across from the database, so we can get passwords
    # --concurrency sets the number of threads to use for concurrent cookbook uploads. Default to 10, but the user can adjust if they desire.

    cmd = "/opt/opscode/embedded/bin/knife ec restore --skip-useracl --with-user-sql --concurrency #{@options.upload_threads} -c /tmp/knife-ec-backup-config.rb #{ec_data_dir}"
    #cmd = "/opt/opscode/embedded/bin/knife ec restore --skip-useracl --concurrency 1 -c /tmp/knife-ec-backup-config.rb #{ec_data_dir}"
    #cmd = "/opt/opscode/embedded/bin/knife ec restore --skip-useracl --with-user-sql --concurrency 1 -c /tmp/knife-ec-backup-config.rb -VV #{ec_data_dir}"
    status = run_command(cmd)
    msg = "Uploading transformed data to the Enterprise Chef server failed"
    check_status(status, msg)
  end

  run_upgrade()
end
