require 'singleton'
class Secrets
  include Singleton

  def self.get(group, name)
    instance.get(group, name)
  end

  def initialize
    # TODO(ssd) 2017-03-24: This should maybe go in chef-secrets itself
    provider = if ENV['CHEF_SECRETS_DATA']
                 puts "CHEF_SECRETS_DATA is set. Using that directly"
                 'chef-secrets-env'
               elsif ENV['CHEF_SECRETS_FD']
                 puts "CHEF_SECRETS_FD=#{ENV['CHEF_SECRETS_FD']}"
                 'chef-secrets-fd'
               else
                 raise "No CHEF_SECRETS initialization data in the environment."
               end
    @veil = Veil::CredentialCollection.from_config(provider: provider)
  end

  def get(group, name)
    @veil.get(group, name)
  end
end
