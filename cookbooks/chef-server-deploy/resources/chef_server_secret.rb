resource_name :chef_server_secret

# Secret spec in group.name format (e.g. saml.client_id)
property :secret_spec, String, name_property: true
property :value, String

action :set do
  execute "Add secret #{secret_spec}" do
    command ['chef-server-ctl', 'set-secret', secret_spec.split('.'),
             value].flatten
    not_if "/opt/opscode/embedded/bin/veil-env-helper -s #{secret_spec} true"
  end
end
