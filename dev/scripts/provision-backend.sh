cat > /etc/chef-backend/chef-backend.rb <<EOF
publish_address "#{IPS[:be]}"
EOF
chef-backend-ctl create-cluster --accept-license
chef-backend-ctl gen-server-config api.chef-server.dev > /installers/api.chef-server.dev.rb
