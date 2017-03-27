require "veil"
require "time"
require "optparse"
require "highline"

# rotate-credentials will generate new credential values for all credentials for
# a given service by incrementing the value and creating a new hash value.
# Because the shared secrets are known on all nodes in the cluster the user can
# choose between copying the secrets file to each node in the cluster and
# reconfiguring or by running this command on all nodes.
add_command_under_category "rotate-credentials", "Secrets Management", "Rotate Chef Server credentials for a given service", 2 do
  ensure_configured!

  OptionParser.new do |opts|
    opts.banner = "Usage: chef-server-ctl rotate-credentials $service_name"

    opts.on("-h", "--help", "Show this message") do
      puts opts
      exit
    end
  end.parse!(ARGV)

  # Rotate and save the credentials
  begin
    service = ARGV[3]

    unless service
      log("You must provide a credential service name", :error)
      exit(1)
    end

    backup_file = backup_secrets_file

    log("Rotating #{service}'s credentials...", :notice)
    credentials.rotate(service)
    credentials.save
  rescue => e
    # Rollback and exit before we try and apply potentially broken credentials
    restore_secrets_file(backup_file)
    log(e.message, :error)
    exit(1)
  end

  # Apply the changes
  begin
    status = run_chef("#{base_path}/embedded/cookbooks/dna.json")
    if status.success?
      remove_backup_file(backup_file)
      log("#{service}'s credentials have been rotated!", :notice)
      log("Run 'chef-server-ctl rotate-credentials #{service}' on each Chef Server", :notice)
      exit(0)
    else
      log("Credential rotation failed", :error)
      exit(1)
    end
  rescue => e
    log(e.message, :error)
    exit(1)
  end
end

# rotate-all-credentials will generate new credential values for all service
# credentials by incrementing the version and creating a new hash value. Because
# the shared secrets are known on all nodes in the cluster the user can choose
# between copying the secrets file to each node in the cluster and reconfiguring
# or by running this command on all nodes.
add_command_under_category "rotate-all-credentials", "Secrets Management", "Rotate all Chef Server service credentials", 2 do
  ensure_configured!

  # Rotate and save the credentials
  begin
    backup_file = backup_secrets_file

    log("Rotating all Chef Server service credentials...", :notice)
    credentials.rotate_credentials
    credentials.save
  rescue => e
    # Rollback and exit before we try and apply potentially broken credentials
    restore_secrets_file(backup_file)
    log(e.message, :error)
    exit(1)
  end

  # Apply the changes
  begin
    status = run_chef("#{base_path}/embedded/cookbooks/dna.json")
    if status.success?
      remove_backup_file(backup_file)
      log("All credentials have been rotated!", :notice)
      log("Run 'chef-server-ctl rotate-all-credentials' on each Chef Server", :notice)
      exit(0)
    else
      log("Credential rotation failed", :error)
      exit(1)
    end
  rescue => e
    log(e.message, :error)
    exit(1)
  end
end

# rotate-shared-secrets will create a new shared secret and salt and generate
# new service credentials for all services. It will then do a chef run to apply
# the new credentials. As the shared secret and salt is securely and randomly
# generated the user must copy the secrets file to all nodes in the cluster.
add_command_under_category "rotate-shared-secrets", "Secrets Management", "Rotate the Chef Server shared secrets and all service credentials", 2 do
  ensure_configured!

  backup_file = backup_secrets_file

  # Rotate and save the credentials
  begin
    log("Rotating shared credential secrets and service credentials...", :notice)
    credentials.rotate_hasher
    credentials.save
  rescue => e
    # Rollback and exit before we try and apply potentially broken credentials
    restore_secrets_file(backup_file)
    log(e.message, :error)
    exit(1)
  end

  # Apply the changes
  begin
    File.delete(credential_rotation_required_file) if File.exist?(credential_rotation_required_file)
    status = run_chef("#{base_path}/embedded/cookbooks/dna.json")
    if status.success?
      remove_backup_file(backup_file)
      log("The shared secrets and all service credentials have been rotated!", :notice)
      log("Please copy #{secrets_file_path} to each Chef Server and run 'chef-server-ctl reconfigure'", :notice)
      exit(0)
    else
      log("Shared credential rotation failed", :error)
      exit(1)
    end
  rescue => e
    log(e.message, :error)
    exit(1)
  end
end

add_command_under_category "show-service-credentials", "Secrets Management", "Show the service credentials", 2 do
  ensure_configured!

  pp(credentials.legacy_credentials_hash)
  exit(0)
end

# require-credential-rotation is designed to put the Chef Server in a state
# where it's offline and requires complete credential rotation to restart.
# This sort of thing is useful if you're creating public Chef Server images
# and you want to make sure that the Chef Server doesn't start up until all
# secrets have been rotated. It's important to note that credential rotation
# does not rotate the pivotal, user or client keys, or remove any Chef Server
# policy or cookbooks that have been uploaded. When the user rotates the shared
# credentials the chef-client reconfigure run will re-enable/link the services,
# restart the Chef Server and remove the sentinel file that enables the
# pre-hook.
add_command_under_category "require-credential-rotation", "Secrets Management", "Disable the Chef Server and require credential rotation", 2 do
  @agree_to_disable = false
  @ui = HighLine.new

  OptionParser.new do |opts|
    opts.banner = "Usage: chef-server-ctl require-credential-rotation [--yes]"

    opts.on("-h", "--help", "Show this message") do
      puts opts
      exit
    end

    opts.on("-y", "--yes", "Agree to disable the Chef Server and require credential rotation") do
      @agree_to_disable = true
    end
  end.parse!(ARGV)

  # Agree to disable and require rotation
  msg = "Are you sure you want to disable the Chef Server and require credential rotation?" \
          "\n Type 'yes' to confirm: "
  exit(1) unless @agree_to_disable || @ui.ask("<%= color(%Q(#{msg}), :yellow) %>") =~ /yes/i

  # Shut down Chef Server
  run_sv_command("stop")

  # Disable services
  get_all_services.each do |service|
    File.unlink("#{service_path}/#{service}") if service_enabled?(service)
  end

  # Enable the credential rotation pre-hook by touching the enable file
  FileUtils.mkdir_p(data_path) unless File.directory?(data_path)
  FileUtils.touch(credential_rotation_required_file)

  log("The Chef Server has been disabled until credentials have been rotated. "\
      "Run 'sudo chef-server-ctl rotate-shared-secrets' to rotate them.")

  exit(0)
end

add_global_pre_hook "require_credential_rotation" do
  # exit if credential rotation is not required
  return unless File.exist?(credential_rotation_required_file)

  # Allow running "chef-server-ctl rotate-shared-secrets"
  # "chef-server-ctl" is a wapper that runs "omnibus-ctl opscode $command"
  # so we'll look for that in ARGV
  return if ARGV == %w{omnibus-ctl opscode rotate-shared-secrets}

  raise("You must rotate the Chef Server credentials to enable the Chef Server. "\
        "Please run 'sudo chef-server-ctl rotate-shared-secrets'")
end

def backup_secrets_file(backup_file = nil)
  timestamp = Time.now.iso8601
  backup_file ||= File.expand_path(File.join(File.dirname(secrets_file_path), "#{File.basename(secrets_file_path)}#{timestamp}.json"))
  log("Backing up #{secrets_file_path} to #{backup_file}...")
  FileUtils.cp(secrets_file_path, backup_file)
  backup_file
end

def restore_secrets_file(backup_file)
  log("Restoring #{backup_file} to #{secrets_file_path}...")
  FileUtils.cp(secrets_file_path, backup_file)
end

def secrets_file_path
  "/etc/opscode/private-chef-secrets.json"
end

def remove_backup_file(backup_file)
  log("Removing #{backup_file}...")
  FileUtils.rm(backup_file)
end

def credential_rotation_required_file
  File.join(data_path, "credential_rotation_required")
end

def ensure_configured!
  unless running_config
    log("You must reconfigure the Chef Server before a backup can be performed", :error)
    exit(1)
  end
end

def log(message, level = :info)
  case level
  when :info
    $stdout.puts message
  when :notice
    $stdout.puts "\e[32m#{message}\e[0m"
  when :error
    $stderr.puts "\e[31m[ERROR]\e[0m #{message}"
  end
end
