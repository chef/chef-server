# apt-get update -y
# apt-get -y install build-essential binutils-doc autoconf flex bison git-core openjdk-6-jdk default-jdk ruby ruby1.8 ruby1.8-dev rdoc1.8 irb1.8 ri1.8 libopenssl-ruby1.8 rubygems libtool dpkg-dev libxml2 libxml2-dev libxslt1.1 libxslt1-dev help2man gettext texinfo
# update-java-alternatives -s java-6-openjdk
# gem install fpm ohai --no-rdoc --no-ri
# ln -s /var/lib/gems/1.8/bin/* /usr/local/bin

# make certain our chef-solo cache dir exists
directory "#{Chef::Config[:file_cache_path]}" do
  action :create
end

case node['platform']
when "ubuntu"
  include_recipe "apt"
when "centos"
  include_recipe "yum"
end

include_recipe "build-essential"
include_recipe "git"
include_recipe "python"

# install ruby and symlink the binaries to /usr/local
include_recipe "ruby_1.9"
%w{ruby gem rake bundle fpm}.each do |bin|
  link "/usr/local/bin/#{bin}" do
    to "/opt/ruby1.9/bin/#{bin}"
  end
end

# install the packaging related packages
package_pkgs = value_for_platform(
  ["ubuntu"] => {
    "default" => ["dpkg-dev"]
  },
  ["centos"] => {
    "default" => ["rpm-build"]
  }
)
package_pkgs.each do |pkg|
  package pkg do
    action :install
  end
end

# install the libxml / libxslt packages
xml_pkgs = value_for_platform(
  ["ubuntu"] => {
    "default" => ["libxml2", "libxml2-dev", "libxslt1.1", "libxslt1-dev"]
  },
  ["centos"] => {
    "default" => ["libxml2", "libxml2-devel", "libxslt", "libxslt-devel"]
  }
)
xml_pkgs.each do |pkg|
  package pkg do
    action :install
  end
end

%w{libtool help2man gettext texinfo}.each do |name|
  package name
end

# centos6 installs matahari which pulls in an AMQP broker called qpid which fights with rabbitmq over its port
%w{qpid-cpp-client qpid-cpp-server qpid-cpp-client-ssl qpid-cpp-server-ssl qpid-qmf}.each do |pkg|
  package pkg do
    action :remove
  end
end

bash "install python packages" do
  code <<BASH
pip install Sphinx==1.1.3
pip install Pygments==1.4
BASH
end

directory "/opt/opscode" do
  mode "755"
  owner node["opscode-omnibus"]["build-user"]
  recursive true
end

directory "/opt/chef" do
  mode "755"
  owner node["opscode-omnibus"]["build-user"]
  recursive true
end

directory "/var/cache/omnibus" do
  mode "755"
  owner node["opscode-omnibus"]["build-user"]
  recursive true
end

# gtihub's SSH key
directory File.expand_path("~#{node["opscode-omnibus"]["build-user"]}/.ssh") do
  owner node["opscode-omnibus"]["build-user"]
  mode "700"
end

file File.expand_path("~#{node["opscode-omnibus"]["build-user"]}/.ssh/known_hosts") do
  action :create_if_missing
  content "github.com,207.97.227.239 ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAq2A7hRGmdnm9tUDbO9IDSwBK6TbQa+PXYPCPy6rbTrTtw7PHkccKrpp0yVhp5HdEIcKr6pLlVDBfOLX9QUsyCOV0wzfjIJNlGEYsdlLJizHhbn2mUjvSAHQqZETYP81eFzLQNnPHt4EVVUh7VfDESU84KezmD5QlWpXLmvU31/yMf+Se8xhHTvKSCZIFImWwoG6mbUoWf9nzpIoaSjB+weqqUUmpaaasXVal72J+UX2B+2RPW3RcT0eOzQgqlJL3RKrTJvdsjE3JEAvGq3lGHSZXy28G3skua2SmVi/w4yCE6gbODqnTWlg7+wC604ydGXA8VJiS5ap43JXiUFFAaQ==\n"
  owner node["opscode-omnibus"]["build-user"]
  mode "600"
end
