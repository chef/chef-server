require "veil"
require "time"
require "optparse"
require "highline"

add_command_under_category "rotate-credentials", "credential-rotation", "Rotate Chef Server credentials for a given service", 2 do
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
    credentials = Veil::CredentialCollection::ChefSecretsFile.from_file(secrets_file_path)
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

add_command_under_category "rotate-all-credentials", "credential-rotation", "Rotate all Chef Server service credentials", 2 do
  ensure_configured!

  # Rotate and save the credentials
  begin
    backup_file = backup_secrets_file

    log("Rotating all Chef Server service credentials...", :notice)
    credentials = Veil::CredentialCollection::ChefSecretsFile.from_file(secrets_file_path)
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

add_command_under_category "rotate-shared-secrets", "credential-rotation", "Rotate the Chef Server shared secrets and all service credentials", 2 do
  ensure_configured!

  backup_file = backup_secrets_file

  # Rotate and save the credentials
  begin
    log("Rotating shared credential secrets and service credentials...", :notice)
    credentials = Veil::CredentialCollection::ChefSecretsFile.from_file(secrets_file_path)
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
    File.delete(credential_prehook_file_path) if File.exist?(credential_prehook_file_path)
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

add_command_under_category "show-service-credentials", "credential-rotation", "Show the service credentials", 2 do
  ensure_configured!

  credentials = Veil::CredentialCollection::ChefSecretsFile.from_file(secrets_file_path)
  pp(credentials.legacy_credentials_hash)
  exit(0)
end

add_command_under_category "require-credential-rotation", "credential-rotation", "Disable the Chef Server and require credential rotation", 2 do
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

  # Create global pre hook that'll prevent ctl commands from running until the
  # credentials have been rotated
  File.open(credential_prehook_file_path, "a+") do |file|
    file.write <<-EOF.gsub(/^\s{6}/, "")
      add_global_pre_hook "require_credential_rotation" do
        # Allow running "chef-server-ctl rotate-shared-secrets"
        # "chef-server-ctl" is a wapper that runs "omnibus-ctl opscode $command"
        # so we'll look for our command and opscode in the args

        return true if ARGV.include?("opscode") && ARGV.include?("rotate-shared-secrets")

        raise("You must rotate the Chef Server credentials to enable the Chef Server. "\
              "Please run 'sudo chef-server-ctl rotate-shared-secrets'")
      end
    EOF
  end

  exit(0)
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

def credential_prehook_file_path
  File.expand_path(File.join(__FILE__, "../", "require_credential_rotation_hook.rb"))
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
