#!{{pkgPathFor "core/ruby"}}/bin/ruby

require 'toml'
require 'openssl'
require 'securerandom'

secrets = TOML.load_file('{{pkg.svc_config_path}}/hab-secrets-config.toml')
new_secrets = secrets.dup
new_secrets['secrets'].each do |service, value|
  value.each do |name, _pass|
    puts "[DEBUG] service #{service} valname #{name} valpass #{_pass}"
    if _pass.empty?
      new_secrets['secrets'][service][name] = name.end_with?('_key') ? OpenSSL::PKey::RSA.generate(2048).to_s : SecureRandom.hex(32)
      puts "[DEBUG] DID: #{new_secrets['secrets'][service][name]}"
    end
  end
end

File.write('{{pkg.svc_data_path}}/hab-secrets-modified.toml', TOML::Generator.new(new_secrets).body)
system "hab config apply chef-server-ctl.default #{Time.now.getutc.to_i} {{pkg.svc_data_path}}/hab-secrets-modified.toml"
