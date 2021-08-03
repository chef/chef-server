#!{{pkgPathFor "core/ruby27"}}/bin/ruby

require "toml"
require "openssl"
require "securerandom"

REQUIRED_SECRETS = {
  postgresql: {
    db_superuser_password: { length: 100 },
  },
  redis_lb: {
    password: { length: 100 },
  },
  keepalived: {
    vrrp_instance_password: { length: 100 },
  },
  opscode_erchef: {
    sql_password: { length: 60 },
    sql_ro_password: { length: 60 },
    stats_password: { length: 100 },
  },
  oc_bifrost: {
    superuser_id: { length: 32 },
    sql_password: { length: 100 },
    sql_ro_password: { length: 100 },
  },
  oc_id: {
    secret_key_base: { length: 100 },
    sql_password: { length: 100 },
    sql_ro_password: { length: 100 },
  },
  bookshelf: {
    access_key_id: { length: 40 },
    secret_access_key: { length: 80 },
    sql_password: { length: 80 },
    sql_ro_password: { length: 80 },
  },
  'chef-server': {
                   superuser_id: { length: 32 },
                   superuser_key: { length: 2048, type: "rsa", private: true, pub_key_name: "superuser_pub_key" },
                   superuser_pub_key: { type: "rsa", private: false },
                   webui_key: { length: 2048, type: "rsa", private: true, pub_key_name: "webui_pub_key" },
                   webui_pub_key: { type: "rsa", private: false },
                 },
  'push-jobs-server': {
                        pushy_priv_key: { length: 2048, type: "rsa", private: true, pub_key_name: "pushy_pub_key" },
                        pushy_pub_key: { type: "rsa", private: false },
                        sql_password: { length: 60 },
                        sql_ro_password: { length: 60 },
                      },
}

def secrets_apply_loop
  toml_cfg = TOML.load_file("/hab/svc/chef-server-ctl/config/hab-secrets-config.toml")
  new_secrets = Marshal.load(Marshal.dump(toml_cfg))
  changes_to_apply = false

  toml_cfg.each do |top_level_item, top_level_item_value|
    # guard against iterating through non-secrets key in default.toml
    next unless top_level_item == "secrets"

    top_level_item_value.each do |service_item, service_item_value|
      service_item_value.each do |key, pass|
        if pass.empty?
          changes_to_apply = true
          if REQUIRED_SECRETS[service_item.to_sym][key.to_sym].has_key?(:type) && REQUIRED_SECRETS[service_item.to_sym][key.to_sym][:type] == "rsa"
            priv_key = OpenSSL::PKey::RSA.generate(2048)
            pub_key = OpenSSL::PKey::RSA.new(priv_key).public_key.to_s
            if REQUIRED_SECRETS[service_item.to_sym][key.to_sym][:private]
              new_secrets[top_level_item][service_item][key] = priv_key.to_s
              pub_key_name = REQUIRED_SECRETS[service_item.to_sym][key.to_sym][:pub_key_name]
              new_secrets[top_level_item][service_item][pub_key_name] = pub_key
              puts "Updated Private/Public Keypair for #{service_item}/#{key}"
            end
          else
            length = REQUIRED_SECRETS[service_item.to_sym][key.to_sym][:length].to_i
            new_secrets[top_level_item][service_item][key] = SecureRandom.hex(length)[1..length]
            puts "Updated Empty Key/Value: #{service_item}/#{key} #{new_secrets[top_level_item][service_item][key]}"
          end
        end
      end
    end
  end

  if changes_to_apply
    puts "Changed Secrets need to be applied."
    File.write("{{pkg.svc_data_path}}/hab-secrets-modified.toml", TOML::Generator.new(new_secrets).body)
    version = Time.now.getutc.to_i
    cmd = "hab config apply chef-server-ctl.default #{version} {{pkg.svc_data_path}}/hab-secrets-modified.toml"
    sup_listen_ctl = ENV["HAB_LISTEN_CTL"]
    cmd += " --remote-sup #{sup_listen_ctl}" if sup_listen_ctl
    system cmd
  else
    puts "Secrets Unchanged - nothing to do."
  end
end

# forever loop
loop do
  secrets_apply_loop
  sleep 15
end
