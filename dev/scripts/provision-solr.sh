add-apt-repository ppa:openjdk-r/ppa

apt-get update

apt-get install openjdk-8-jdk -y
export JAVA_HOME=/usr/lib/jvm/java-1.8.0-openjdk-amd64

version="4.10.4"
wget http://archive.apache.org/dist/lucene/solr/$version/solr-$version.tgz

tar xzf solr-$version.tgz 

chmod 777 solr-4.10.4/example/solr/collection1/conf/schema.xml
chmod 777 solr-4.10.4/example/solr/solr.xml

solr-$version/bin/solr start
