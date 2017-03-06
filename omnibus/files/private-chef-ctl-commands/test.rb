#
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

add_command_under_category "test", "general", "Run the API test suite against localhost.", 2 do
  require 'veil'

  veil = Veil::CredentialCollection::ChefSecretsFile.from_file("/etc/opscode/private-chef-secrets.json")
  ENV['SUPERUSER_KEY'] = veil.get("chef-server", "superuser_key")
  ENV['WEBUI_KEY'] = veil.get("chef-server", "webui_key")

  pedant_args = ARGV[3..-1]
  pedant_args = ["--smoke"] unless pedant_args.any?
  Dir.chdir(File.join(base_path, "embedded", "service", "oc-chef-pedant"))
  pedant_config = File.join(data_path, "oc-chef-pedant", "etc", "pedant_config.rb")
  bundle = File.join(base_path, "embedded", "bin", "bundle")
  status = run_command("#{bundle} exec ./bin/oc-chef-pedant -c #{pedant_config} #{pedant_args.join(' ')}")
  exit status.exitstatus
end
