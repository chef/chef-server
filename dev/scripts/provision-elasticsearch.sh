add-apt-repository -y ppa:openjdk-r/ppa
wget -qO - https://artifacts.elastic.co/GPG-KEY-elasticsearch | sudo apt-key add -

if [ "${ELASTIC_VERSION}" = "2" ]; then
  echo "deb https://packages.elastic.co/elasticsearch/2.x/debian stable main" | sudo tee -a /etc/apt/sources.list.d/elasticsearch-2.x.list
elif [ "${ELASTIC_VERSION}" = "5" ]; then
  echo "deb https://artifacts.elastic.co/packages/5.x/apt stable main" | sudo tee -a /etc/apt/sources.list.d/elastic-5.x.list
elif [ "${ELASTIC_VERSION}" = "7" ]; then
  echo "deb https://artifacts.elastic.co/packages/7.x/apt stable main" | sudo tee -a /etc/apt/sources.list.d/elastic-7.x.list
else
  echo "deb https://artifacts.elastic.co/packages/6.x/apt stable main" | sudo tee -a /etc/apt/sources.list.d/elastic-6.x.list
fi

apt-get update

apt-get install openjdk-8-jdk openjdk-8-jre openjdk-8-jre-headless openjdk-8-jdk-headless -y
export JAVA_HOME=/usr/lib/jvm/java-1.8.0-openjdk-amd64

apt-get install elasticsearch -y

mkdir -p /var/data/elasticsearch
chown elasticsearch: /var/data/elasticsearch

cat > /etc/elasticsearch/elasticsearch.yml <<'EOF'
http.host: 0.0.0.0
path.data: /var/data/elasticsearch
path.logs: /var/log/elasticsearch
EOF

sed -i '/START_DAEMON/{s/^#//;}' /etc/default/elasticsearch

systemctl restart elasticsearch

sleep 30

# check to see that elasticsearch started properly
netstat -nap | grep -q java || { echo 'ERROR: Elasticsearch is NOT running!'; exit 1; }
