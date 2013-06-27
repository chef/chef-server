#
# Author:: Seth Chisamore (<schisamo@opscode.com>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved

bookshelf_dir = node['private_chef']['bookshelf']['dir']
bookshelf_etc_dir = File.join(bookshelf_dir, "etc")
bookshelf_log_dir = node['private_chef']['bookshelf']['log_directory']
bookshelf_sasl_log_dir = File.join(bookshelf_log_dir, "sasl")
bookshelf_data_dir = node['private_chef']['bookshelf']['data_dir']
[
  bookshelf_dir,
  bookshelf_etc_dir,
  bookshelf_log_dir,
  bookshelf_sasl_log_dir,
  bookshelf_data_dir,
].each do |dir_name|
  directory dir_name do
    owner node['private_chef']['user']['username']
    mode '0700'
    recursive true
  end
end

link "/opt/opscode/embedded/service/bookshelf/log" do
  to bookshelf_log_dir
end

template "/opt/opscode/embedded/service/bookshelf/bin/bookshelf" do
  source "bookshelf.erb"
  owner "root"
  group "root"
  mode "0755"
  variables(node['private_chef']['bookshelf'].to_hash)
  notifies :restart, 'service[bookshelf]' if OmnibusHelper.should_notify?("bookshelf")
end

bookshelf_config = File.join(bookshelf_etc_dir, "app.config")

template bookshelf_config do
  source "bookshelf.config.erb"
  mode "644"
  variables(node['private_chef']['bookshelf'].to_hash)
  notifies :restart, 'service[bookshelf]' if OmnibusHelper.should_notify?("bookshelf")
end

link "/opt/opscode/embedded/service/bookshelf/etc/app.config" do
  to bookshelf_config
end

component_runit_service "bookshelf"
