require "chef_backup"
require "chef/mixin/deep_merge"
require "optparse"
require "ostruct"

add_command_under_category "backup", "general", "Backup the Chef Infra Server", 2 do
  ensure_configured!
  ensure_rsync!

  options = OpenStruct.new
  options.agree_to_go_offline = false
  options.config_only = false

  OptionParser.new do |opts|
    opts.banner = "Usage: chef-server-ctl backup [options]"

    opts.on("-y", "--yes", "Agree to go offline during tar based backups") do
      options.agree_to_go_offline = true
    end

    opts.on("--pg-options [string]", "Additional options to pass to PostgreSQL during backups") do |pg_options|
      options.pg_options = pg_options
    end

    opts.on("-c", "--config-only", "Only backup configuration, no data") do
      options.config_only = true
    end

    opts.on("-t", "--timeout [string]", "Set the Maximum amount of time to wait for shell commands") do |shell_out_timeout|
      options.shell_out_timeout = shell_out_timeout
    end

    opts.on("-h", "--help", "Show this message") do
      puts opts
      exit
    end
  end.parse!(ARGV)

  Chef::Mixin::DeepMerge.deep_merge!(stringify_keys(options.to_h), running_config["private_chef"]["backup"])

  begin
    with_temp_dir { ChefBackup::Runner.new(running_config).backup }
  rescue => e
    log(e.message, :error)
    exit(1)
  end

  exit(0)
end

add_command_under_category "restore", "general", "Restore the Chef Infra Server from backup", 2 do
  ensure_rsync!

  options = OpenStruct.new
  options.agree_to_cleanse = nil
  options.restore_dir = nil

  OptionParser.new do |opts|
    opts.banner = "Usage: chef-server-ctl restore $PATH_TO_BACKUP_TARBALL [options]"

    opts.on("-d", "--staging-dir [directory]", String, "The path to an empty directory for use in restoration.  Ensure it has enough available space for all expanded data in the backup archive") do |staging_directory|
      options.restore_dir = File.expand_path(staging_directory)
    end

    opts.on("-c", "--cleanse", "Agree to cleansing all existing state during a restore.  THIS WILL COMPLETELY REMOVING EXISTING CHEF DATA") do
      options.agree_to_cleanse = "yes"
    end

    opts.on("--pg-options [string]", "Additional options to pass to PostgreSQL during backups") do |pg_options|
      options.pg_options = pg_options
    end

    opts.on("-t", "--timeout [string]", "Maximum amount of time to wait for shell commands") do |shell_out_timeout|
      options.shell_out_timeout = shell_out_timeout
    end

    opts.on("-h", "--help", "Show this message") do
      puts opts
      exit
    end
  end.parse!(ARGV)

  unless ARGV.length >= 2
    log("Invalid command", :error)
    log("USAGE: chef-server-ctl restore $PATH_TO_BACKUP_TARBALL [options]", :error)
    exit(1)
  end

  config = stringify_keys(options.to_h)
  config["restore_param"] = normalize_arg(ARGV[1])

  begin
    with_temp_dir { ChefBackup::Runner.new(config).restore }
  rescue => e
    log(e.message, :error)
    exit(1)
  end

  exit(0)
end

def ensure_rsync!
  log("Locating rsync..")
  unless system("which rsync")
    log("rsync must be installed in order to run this command", :error)
    exit(1)
  end
end

def ensure_configured!
  unless running_config
    log("You must reconfigure the Chef Server before a backup can be performed", :error)
    exit(1)
  end
end

def with_temp_dir
  Dir.mktmpdir do |dir|
    Dir.chdir(dir) do
      yield
    end
  end
end

def stringify_keys(hash)
  hash.keys.each { |k| hash[k.to_s] = hash.delete(k) }
  hash
end

def normalize_arg(arg)
  arg =~ /^snap-\h{8}$/ ? arg : File.expand_path(arg)
end

def log(message, level = :info)
  case level
  when :info
    puts message
  when :error
    $stderr.puts "\e[31m[ERROR]\e[0m #{message}"
  end
end
