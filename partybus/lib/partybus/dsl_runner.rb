require 'partybus/migration_api/v1'

class Partybus::DSLRunner

  attr_reader :api_version

  def define_upgrade(options={}, &block)
    @api_version = options[:api_version] || :v1

    # TODO: use the API version to load the correct UpgradeAPI class
    # we need an UpgradeAPIFactory :D
    Partybus::UpgradeAPI::V1.new(&block)
  end

end
