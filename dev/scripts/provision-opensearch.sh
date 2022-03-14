curl -o /tmp/opensearch-1.2.4-linux-x64.tar.gz https://artifacts.opensearch.org/releases/bundle/opensearch/1.2.4/opensearch-1.2.4-linux-x64.tar.gz
sudo tar -zxf /tmp/opensearch-1.2.4-linux-x64.tar.gz -C /etc/
cd /etc/opensearch-1.2.4

./opensearch-tar-install.sh &
PID=$!
sleep 20
kill $PID

cat > /etc/opensearch-1.2.4/config/opensearch.yml <<'EOF'
# WARNING: revise all the lines below before you go into production
plugins.security.ssl.transport.pemcert_filepath: esnode.pem
plugins.security.ssl.transport.pemkey_filepath: esnode-key.pem
plugins.security.ssl.transport.pemtrustedcas_filepath: root-ca.pem
plugins.security.ssl.transport.enforce_hostname_verification: false
plugins.security.ssl.http.enabled: false
plugins.security.ssl.http.pemcert_filepath: esnode.pem
plugins.security.ssl.http.pemkey_filepath: esnode-key.pem
plugins.security.ssl.http.pemtrustedcas_filepath: root-ca.pem
plugins.security.allow_unsafe_democertificates: true
plugins.security.allow_default_init_securityindex: true
plugins.security.authcz.admin_dn:
  - CN=kirk,OU=client,O=client,L=test, C=de

plugins.security.audit.type: internal_opensearch
plugins.security.enable_snapshot_restore_privilege: true
plugins.security.check_snapshot_restore_write_privileges: true
plugins.security.restapi.roles_enabled: ["all_access", "security_rest_api_access"]
plugins.security.system_indices.enabled: true
plugins.security.system_indices.indices: [".opendistro-alerting-config", ".opendistro-alerting-alert*", ".opendistro-anomaly-results*", ".opendistro-anomaly-detector*", ".opendistro-anomaly-checkpoints", ".opendistro-anomaly-detection-state", ".opendistro-reports-*", ".opendistro-notifications-*", ".opendistro-notebooks", ".opendistro-asynchronous-search-response*", ".replication-metadata-store"]
node.max_local_storage_nodes: 3

network.host: "0.0.0.0"
discovery.type: single-node
EOF

./opensearch-tar-install.sh -d

sleep 30

# check to see that opensearch started properly
curl -XGET http://localhost:9200 -u 'admin:admin' --insecure
curl -XGET http://localhost:9200/_cat/plugins?v -u 'admin:admin' --insecure
netstat -nap | grep -q java || { echo 'ERROR: Opensearch is NOT running!'; exit 1; }
