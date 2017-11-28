require 'partybus/migration_api/v1'

class Partybus::DSLRunner

  attr_reader :api_version

  def initialize(migration_file)
    @major_version = migration_file.major
    @minor_version = migration_file.minor
    @file_path     = migration_file.path
  end

  def load_migration
    instance_eval(IO.read(@file_path))
  end

  def run
    load_migration
    run_migration
    write_migration_file
  end

  def check
    load_migration
    run_check
  end

  def write_migration_file
    File.open(Partybus.config.migration_state_file, 'w') do |f|
      f.puts({:major => @major_version, :minor => @minor_version}.to_json)
    end
  end

  def run_migration
    # TODO: use the API version to load the correct UpgradeAPI class
    # we need an UpgradeAPIFactory :D
    Partybus::UpgradeAPI::V1.new(&@block)
  end

  def run_check
    if @check_block
      Partybus::UpgradeAPI::V1.new(&@check_block).eval_result
    else
      # NOTE(ssd) 2017-11-28: The assumption here is that a migration
      # is safe to re-run unless otherwise specififed.
      # Counter-intuitively, we define checks for the safest
      # migrations to re-run (sqitch) because they are easy to check
      # for.
      false
    end
  end

  def define_check(options={}, &block)
    @check_api_version = options[:api_version] || :v1
    @check_block = block
  end

  def define_upgrade(options={}, &block)
    @api_version = options[:api_version] || :v1
    @block = block
  end
end
