class Partybus::DSLRunner

  attr_reader :api_version

  def define_migration(options={}, &block)
    @api_version = options[:api_version] || :v1
  end

end
