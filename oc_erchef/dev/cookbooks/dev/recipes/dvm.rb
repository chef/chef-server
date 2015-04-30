# dvm gem and project setup. This one is ugly...
#


execute "install deep_merge" do
  command <<-EOM
export PATH=/opt/opscode/embedded/bin:$PATH
cd /tmp
git clone https://github.com/danielsdeleo/deep_merge.git
cd deep_merge
rake package
gem install -l pkg/deep_merge-1.0.1.gem
rm -rf /tmp/deep_merge
EOM
  only_if { DVMHelper.dvm_path.nil? }
end

execute "install and load dvm" do
  cwd "/vagrant/dvm"
  command <<-EOM
# Build and install dvm
gem build dvm.gemspec
gem install -l dvm-100.0.0.gem
rm dvm-100.0.0.gem
EOM
  only_if { DVMHelper.dvm_path.nil? }
end

mount "dvm" do
  mount_point lazy { DVMHelper.dvm_path }
  device "/vagrant/dvm"
  fstype "none"
  options "bind"
end
