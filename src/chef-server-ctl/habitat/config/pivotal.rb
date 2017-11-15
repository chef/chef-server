node_name "pivotal"
chef_server_url "https://chef-server-nginx:443"
chef_server_root "https://chef-server-nginx:443"
no_proxy "127.0.0.1"
client_key "{{pkg.svc_config_path}}/pivotal.pem"
ssl_verify_mode :verify_none
