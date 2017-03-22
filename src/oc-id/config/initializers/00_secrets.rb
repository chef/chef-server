require 'singleton'
class Secrets
  include Singleton

  def self.get(group, name)
    instance.get(group, name)
  end

  def initialize
    @veil = Veil::CredentialCollection.from_config(provider: 'chef-secrets-fd')
  end

  def get(group, name)
    @veil.get(group, name)
  end
end
