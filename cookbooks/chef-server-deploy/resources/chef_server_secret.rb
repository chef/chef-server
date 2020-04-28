resource_name :chef_server_secret

# Secret spec in group.name format (e.g. saml.client_id)
property :secret_spec, String, name_property: true
property :value, String

action :set do
  execute "Add secret #{new_resource.secret_spec}" do
    command ["#{Chef::Dist::Server::CTL}", 'set-secret', new_resource.secret_spec.split('.'),
             new_resource.value].flatten
    not_if "/opt/opscode/embedded/bin/veil-env-helper -s #{new_resource.secret_spec} true"
  end
end
