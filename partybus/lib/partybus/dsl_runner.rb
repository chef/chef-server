require 'partybus/migration_api/v1'

class Partybus::DSLRunner

  attr_reader :api_version

  def initialize(migration_file)
    @major_version = migration_file.major
    @minor_version = migration_file.minor
    @file_path     = migration_file.path
  end

  def run
    instance_eval(IO.read(@file_path))
    File.open(Partybus.config.migration_state_file, 'w') do |f|
      f.puts({:major => @major_version, :minor => @minor_version}.to_json)
    end
  end

  def define_upgrade(options={}, &block)
    @api_version = options[:api_version] || :v1

    # TODO: use the API version to load the correct UpgradeAPI class
    # we need an UpgradeAPIFactory :D
    Partybus::UpgradeAPI::V1.new(&block)
  end

end
