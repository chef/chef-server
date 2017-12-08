node_name "pivotal"
chef_server_url "https://{{cfg.chef_server_api.ip}}"
chef_server_root "https://{{cfg.chef_server_api.ip}}"
no_proxy "127.0.0.1"
client_key "{{pkg.svc_config_path}}/pivotal.pem"
ssl_verify_mode :verify_none
