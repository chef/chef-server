node_name "pivotal"
{{~#if bind.chef-server-nginx}}
  {{~#eachAlive bind.chef-server-nginx.members as |member|}}
    {{~#if @last}}
chef_server_url "https://{{member.sys.ip}}:{{member.cfg.ssl-port}}"
chef_server_root "https://{{member.sys.ip}}:{{member.cfg.ssl-port}}"
    {{~/if}}
  {{~/eachAlive}}
{{~else}}
chef_server_url "https://{{cfg.chef_server_api.ip}}:{{cfg.chef_server_api.ssl_port}}"
chef_server_root "https://{{cfg.chef_server_api.ip}}:{{cfg.chef_server_api.ssl_port}}"
{{~/if}}
no_proxy "127.0.0.1"
client_key "{{pkg.svc_config_path}}/pivotal.pem"
ssl_verify_mode :verify_none
