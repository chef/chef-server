node.default['private_chef']['nginx']['enable_non_ssl'] = true
node.default['private_chef']['nginx']['ssl_ciphers'] = "FIPS@STRENGTH:!aNULL:!eNULL"

