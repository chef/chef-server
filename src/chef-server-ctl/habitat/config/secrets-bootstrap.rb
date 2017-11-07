#!{{pkgPathFor "core/ruby"}}/bin/ruby

require 'toml'
require 'openssl'
require 'securerandom'

secrets = TOML.load_file('{{pkg.svc_config_path}}/hab-secrets-config.toml')
new_secrets = {}
secrets.each do |service, value|
  new_secrets[service] = {}
  value.each do |name, _pass|
    name.each do |k, _v|
      new_secrets[service][k] = k.end_with?('_key') ? OpenSSL::PKey::RSA.generate(2048).to_s : SecureRandom.hex(32)
    end
  end
end

File.write('{{pkg.svc_data_path}}/veil-secrets.toml', TOML::Generator.new(new_secrets).body)
system "hab config apply oc_erchef.default #{Time.now.getutc.to_i} {{pkg.svc_data_path}}/veil-secrets.toml"
