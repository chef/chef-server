# CREATED BY CHEF - DO NOT MODIFY

# Make sure your secret_key_base is kept private
# if you're sharing your code publicly.
OcId::Application.config.secret_key_base = "<%= node['private_chef']['oc_id']['secret_key_base'] %>"
