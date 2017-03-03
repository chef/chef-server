# CREATED BY CHEF - DO NOT MODIFY
require 'veil'
veil = Veil::CredentialCollection::ChefSecretsFile.from_file("/etc/opscode/private-chef-secrets.json")
OcId::Application.config.secret_key_base = veil.get('oc_id', 'secret_key_base')
