add_command_under_category "show-secret", "Debug Tools", "Show the value of the given secret in the secret store", 2 do
  require 'veil'

  group = ARGV[3]
  name = ARGV[4]
  veil = Veil::CredentialCollection::ChefSecretsFile.from_file("/etc/opscode/private-chef-secrets.json")
  puts veil.get(group, name)
end
