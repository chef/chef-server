node_name "pivotal"
chef_server_url "https://172.18.0.1"
chef_server_root "https://172.18.0.1"
no_proxy "127.0.0.1"
client_key "{{pkg.svc_config_path}}/pivotal.pem"
ssl_verify_mode :verify_none
