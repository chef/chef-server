require 'partybus/migration_api/v1'

class Partybus::DSLRunner

  attr_reader :api_version
  MigrationStub = Struct.new(:major, :minor)

  def initialize(migration_file)
    @migration = MigrationStub.new(migration_file.minor, migration_file.major)
    @file_path = migration_file.path
  end

  def load_migration
    instance_eval(IO.read(@file_path))
  end

  def run(migration_state)
    load_migration
    run_migration
    migration_state.apply(@migration)
  end

  def check
    load_migration
    run_check
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
