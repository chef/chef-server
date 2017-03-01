# CREATED BY CHEF - DO NOT MODIFY

# Make sure your secret_key_base is kept private
# if you're sharing your code publicly.
# TODO mp 2017-03-01: secret in a file
OcId::Application.config.secret_key_base = "<%= PrivateChef.credentials.get('oc_id', 'secret_key_base') %>"
