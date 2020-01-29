add-apt-repository -y ppa:openjdk-r/ppa
wget -qO - https://artifacts.elastic.co/GPG-KEY-elasticsearch | sudo apt-key add -

if [ "${ELASTIC_VERSION}" = "2" ]; then
  echo "deb https://packages.elastic.co/elasticsearch/2.x/debian stable main" | sudo tee -a /etc/apt/sources.list.d/elasticsearch-2.x.list
elif [ "${ELASTIC_VERSION}" = "5" ]; then
  echo "deb https://artifacts.elastic.co/packages/5.x/apt stable main" | sudo tee -a /etc/apt/sources.list.d/elastic-5.x.list
else
  echo "deb https://artifacts.elastic.co/packages/6.x/apt stable main" | sudo tee -a /etc/apt/sources.list.d/elastic-6.x.list
fi

apt-get update

apt-get install openjdk-8-jdk -y
export JAVA_HOME=/usr/lib/jvm/java-1.8.0-openjdk-amd64

apt-get install elasticsearch -y
echo "http.host: 0.0.0.0" > /etc/elasticsearch/elasticsearch.yml
service elasticsearch start
