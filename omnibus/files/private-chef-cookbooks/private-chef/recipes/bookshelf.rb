#
# Author:: Seth Chisamore (<schisamo@opscode.com>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved

cookbook_migration = "/opt/opscode/embedded/bin/cookbook_migration.sh"

checksum_path = node['private_chef']['opscode-chef']['checksum_path']
data_path = node['private_chef']['bookshelf']['data_dir']

template cookbook_migration do
  source "cookbook_migration.sh.erb"
  owner "root"
  group "root"
  mode "0755"
end

#
# We need to create all of these directories up front
# Note that data_path will not be a subdir of bookshelf_dir in HA configurations
#
bookshelf_dir = node['private_chef']['bookshelf']['dir']
bookshelf_etc_dir = File.join(bookshelf_dir, "etc")
bookshelf_log_dir = node['private_chef']['bookshelf']['log_directory']
bookshelf_sasl_log_dir = File.join(bookshelf_log_dir, "sasl")
[
  bookshelf_dir,
  bookshelf_etc_dir,
  bookshelf_log_dir,
  bookshelf_sasl_log_dir,
  data_path
].each do |dir_name|
  directory dir_name do
    owner OmnibusHelper.new(node).ownership['owner']
    group OmnibusHelper.new(node).ownership['group']
    mode node['private_chef']['service_dir_perms']
    recursive true
  end
end

execute "cookbook migration" do
  command cookbook_migration
  user OmnibusHelper.new(node).ownership['owner']
  not_if { File.exist?("#{data_path}/_%_BOOKSHELF_DISK_FORMAT") }
end

link "/opt/opscode/embedded/service/bookshelf/log" do
  to bookshelf_log_dir
end

bookshelf_config = File.join(bookshelf_dir, "sys.config")

template bookshelf_config do
  source "bookshelf.config.erb"
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode "644"
  variables(node['private_chef']['bookshelf'].to_hash)
  notifies :restart, 'runit_service[bookshelf]' if is_data_master?
end

link "/opt/opscode/embedded/service/bookshelf/sys.config" do
  to bookshelf_config
end

component_runit_service "bookshelf"
