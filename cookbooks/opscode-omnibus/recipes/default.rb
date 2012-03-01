# apt-get update -y
# apt-get -y install build-essential binutils-doc autoconf flex bison git-core openjdk-6-jdk default-jdk ruby ruby1.8 ruby1.8-dev rdoc1.8 irb1.8 ri1.8 libopenssl-ruby1.8 rubygems libtool dpkg-dev libxml2 libxml2-dev libxslt1.1 libxslt1-dev help2man gettext texinfo
# update-java-alternatives -s java-6-openjdk
# gem install fpm ohai --no-rdoc --no-ri
# ln -s /var/lib/gems/1.8/bin/* /usr/local/bin

include_recipe "apt"
include_recipe "build-essential"
include_recipe "git"

%w{ruby ruby1.8 ruby1.8-dev rdoc1.8 irb1.8 ri1.8 libopenssl-ruby1.8 libtool dpkg-dev libxml2 libxml2-dev libxslt1.1 libxslt1-dev help2man gettext texinfo}.each do |name|
  package name
end

bash "install rubygems 1.3.7 from source" do
  cwd "/tmp"
  code <<-INSTALL_RUBYGEMS
wget http://production.cf.rubygems.org/rubygems/rubygems-1.3.7.tgz
tar zxf rubygems-1.3.7.tgz
cd rubygems-1.3.7
ruby setup.rb --no-format-executable
INSTALL_RUBYGEMS
  not_if { ::File.exists? "/usr/bin/gem" }
end

%w{bundler rake}.each do |name|
  gem_package name do
    gem_binary "/usr/bin/gem"
  end
end

ruby_block "make gem symlinks" do
  block do
    Dir['/var/lib/gems/1.8/bin/*'].each do |path|
      r = Chef::Resource::Link.new("/usr/local/bin/#{File.basename(path)}", run_context)
      r.action(:nothing)
      r.to(path)
      r.run_action(:create)
    end
  end
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
