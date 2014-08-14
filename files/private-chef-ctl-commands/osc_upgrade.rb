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

    fix_rabbit_wait_script

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

    # The OSC bits still live on the system - do we delete them here?
    # For example, /opt/chef-server is still in the path, but /opt/opscode is not
    # on dev-vm testing
    # This has the effect of making the default knife, gem, etc the chef-server versions

    # The migration data still lives on the system - it is probably worth while
    # to include an optional step to delete it, if the user specifies this, other
    # wise leave it on the system
  end

  def fix_rabbit_wait_script
    # The wait-for-rabbit script is an open source chef server script that was
    # supposed to wait for rabbitmq to start before starting erchef.
    # It was supposed to look for rabbitmq on an open source system, but due to
    # a mistake looks for rabbit on an enterprise system instead. It has a bail
    # option if it doesn't find enterprise chef server installed, so this hasn't
    # affected open source systems, until now, since the upgrade process puts
    # both the open source and enterprise chef servers on the same system.
    #
    # The assumption has to be that this file will be wrong on any system
    # undergoing an upgrade, because even if fixed, it can't be assumed that the
    # system will be running an up-to-date open source chef server with the fix
    # applied.
    #
    # If the open source server is reconfigured it will restore the bad script
    # and this fix will need to re-applied.
    # If the bad script is on the system, the symptom that shows up is that
    # erchef will log that it is waiting for rabbit to start. This will only
    # happen if enterprise chef has been installed and configured on the system,
    # because the script will look for the enterprise rabbit install.
    # The script is kicked off when the open source server tries to start erchef,
    # as the script is hooked into the runit start process for erchef.
    #
    # This sed command was written to be as portable as possible and to leave no
    # tmp file behind. See:
    # https://stackoverflow.com/questions/5171901/sed-command-find-and-replace-in-file-and-overwrite-file-doesnt-work-it-empties
    # The copy is done to ensure we keep the permissions of the original file
    script = "/opt/chef-server/bin/wait-for-rabbit"
    sed = "sed 's/opscode/chef-server/g' #{script} > #{script}.tmp && cp --no-preserve=mode,ownership #{script}.tmp #{script} && rm #{script}.tmp"
    msg = "Failed to write fix to wait-for-rabbit script"
    check_status(run_command(sed), msg)
  end

  def start_osc
    # Assumption is EC isn't running, since we detected OSC on the system
    log 'Ensuring the Open Source Chef server is started'
    msg = "Unable to start Open Source Chef server, which is needed to complete the upgrade"
    check_status(run_command("chef-server-ctl start"), msg)
    wait_for_ready_server("Open Source Chef")
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

    sql_user_data = db.select(:username, :id, :public_key, :hashed_password, :salt, :hash_type).from(:osc_users)
    sql_users_json =  sql_user_data.all.to_json
    File.open(key_file, 'w') { |file| file.write(sql_users_json)}
  end

  def start_ec
    log "Ensuring all the Enterprise Chef server components are started"
    msg = "Unable to start Enterprise Chef, which is needed to complete the upgrade"
    status = run_command("private-chef-ctl start")
    check_status(status, msg)
    wait_for_ready_server("Enterprise Chef")
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
