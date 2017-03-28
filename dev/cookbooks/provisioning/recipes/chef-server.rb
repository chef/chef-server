# Bare minimum packages for other stuff to work:
execute "apt-get-update" do
  command "apt-get update"
  ignore_failure true
  not_if do
    File.exists?('/var/chef/cache/apt-update-done')
  end
end

file "/var/chef/cache/apt-update-done" do
  action :create
end
package %w{build-essential git}

# Pre-place a known dhparams pem for nginx - this can save from
# 2-5 minutes of startup time.
#
directory "/var/opt/opscode/nginx/ca" do
  recursive  true
end

cookbook_file "/var/opt/opscode/nginx/ca/dhparams.pem" do
  source 'dhparams.dev'
  action :create
end

directory "/etc/opscode" do
  owner "root"
  group "root"
  recursive true
  action :create
end

directory "/etc/opscode-reporting" do
  owner "root"
  group "root"
  recursive true
  action :create
end

template "/etc/opscode-reporting/opscode-reporting.rb" do
  source "opscode-reporting.rb.erb"
  owner "root"
  group "root"
  action :create
  only_if { node['provisioning'].has_key? 'opscode-reporting-config' }
end

