require 'chef_backup'
require 'chef/mixin/deep_merge'
require 'optparse'
require 'ostruct'

add_command_under_category 'backup', 'general', 'Backup the Chef Server', 2 do
  unless running_config
    puts '[ERROR] cannot backup if you haven\'t completed a reconfigure'
    exit 1
  end

  options = OpenStruct.new
  options.agree_to_go_offline = false

  OptionParser.new do |opts|
    opts.banner = 'Usage: chef-server-ctl backup [options]'

    opts.on('-y', '--yes', 'Agree to go offline during tar based backups') do
      options.agree_to_go_offline = true
    end

    opts.on('-h', '--help', 'Show this message') do
      puts opts
      exit
    end
  end.parse!(ARGV)

  Chef::Mixin::DeepMerge.deep_merge!(stringify_keys(options.to_h), running_config['private_chef']['backup'])

  status = ChefBackup::Runner.new(running_config).backup
  exit(status ? 0 : 1)
end

add_command_under_category 'restore', 'general', 'Restore the Chef Server from backup', 2 do
  options = OpenStruct.new
  options.agree_to_cleanse = nil
  options.restore_dir = nil

  OptionParser.new do |opts|
    opts.banner = 'Usage: chef-server-ctl restore $PATH_TO_BACKUP_TARBALL [options]'

    opts.on('-d', '--staging-dir [directory]', String, 'The path to an empty directory for use in restoration.  Ensure it has enough available space for all expanded data in the backup archive') do |staging_directory|
      options.restore_dir = File.expand_path(staging_directory)
    end

    opts.on('-c', '--cleanse', 'Agree to cleansing all existing state during a restore.  THIS WILL COMPLETELY REMOVING EXISTING CHEF DATA') do
      options.agree_to_cleanse = 'yes'
    end

    opts.on('-h', '--help', 'Show this message') do
      puts opts
      exit
    end
  end.parse!(ARGV)

  unless ARGV.length >= 3
    puts 'ERROR: Invalid command'
    puts 'USAGE: chef-server-ctl restore $PATH_TO_BACKUP_TARBALL [options]'
    exit 1
  end

  config = stringify_keys(options.to_h)
  config['restore_param'] = normalize_arg(ARGV[3])

  status = ChefBackup::Runner.new(config).restore
  exit(status ? 0 : 1)
end

def stringify_keys(hash)
  hash.keys.each { |k| hash[k.to_s] = hash.delete(k) }
  hash
end

def normalize_arg(arg)
  arg =~ /^snap-\h{8}$/ ? arg : File.expand_path(arg)
end
