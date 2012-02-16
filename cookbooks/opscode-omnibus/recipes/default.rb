# apt-get update -y
# apt-get -y install build-essential binutils-doc autoconf flex bison git-core openjdk-6-jdk default-jdk ruby ruby1.8 ruby1.8-dev rdoc1.8 irb1.8 ri1.8 libopenssl-ruby1.8 rubygems libtool dpkg-dev libxml2 libxml2-dev libxslt1.1 libxslt1-dev help2man gettext texinfo
# update-java-alternatives -s java-6-openjdk
# gem install fpm ohai --no-rdoc --no-ri
# ln -s /var/lib/gems/1.8/bin/* /usr/local/bin

include_recipe "apt"
include_recipe "build-essential"
include_recipe "git"
include_recipe "java"

%w{ruby ruby1.8 ruby1.8-dev rdoc1.8 irb1.8 ri1.8 libopenssl-ruby1.8 rubygems libtool dpkg-dev libxml2 libxml2-dev libxslt1.1 libxslt1-dev help2man gettext texinfo}.each do |name|
  package name
end

%w{fpm ohai rake}.each do |name|
  gem_package name do
    gem_binary "/usr/bin/gem"
  end
end

gem_package "mixlib-shellout" do
  gem_binary "/usr/bin/gem"
  options "--pre"
  version "~>1.0"
end

Dir['/var/lib/gems/1.8/bin/*'].each do |path|
  link "/usr/local/bin/#{File.basename(path)}" do
    to path
  end
end

directory "/opt/opscode" do
  mode "777"
  recursive true
end
