#
# Author:: Seth Chisamore (<schisamo@chef.io>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved

cookbook_migration = "/opt/opscode/embedded/bin/cookbook_migration.sh"

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
bookshelf_log_dir = node['private_chef']['bookshelf']['log_directory']
bookshelf_sasl_log_dir = File.join(bookshelf_log_dir, "sasl")
[
  bookshelf_dir,
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

bookshelf_params = node['private_chef']['bookshelf'].to_hash.dup # is dup implicit in #to_hash?
bookshelf_params['postgresql'] = node['private_chef']['postgresql'].dup

template bookshelf_config do
  source "bookshelf.config.erb"
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode "644"
  variables(bookshelf_params)
  notifies :restart, 'runit_service[bookshelf]' if is_data_master?
end

link "/opt/opscode/embedded/service/bookshelf/sys.config" do
  to bookshelf_config
end

vmargs_config = File.join(bookshelf_dir, "vm.args")

template vmargs_config do
  source "bookshelf.vm.args.erb"
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode "644"
  notifies :restart, 'runit_service[bookshelf]' if is_data_master?
end

link "/opt/opscode/embedded/service/bookshelf/vm.args" do
  to vmargs_config
end

component_runit_service "bookshelf"
