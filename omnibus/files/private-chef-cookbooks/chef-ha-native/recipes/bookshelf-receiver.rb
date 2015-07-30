# Copyright 2014 Chef Software Inc
#
# Licensed under the Apache License, Version 2.0 (the "License"); you
# may not use this file except in compliance with the License.  You
# may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
# implied.  See the License for the specific language governing
# permissions and limitations under the License.

bookshelf_receiver_dir = node['private_chef']['bookshelf-receiver']['dir']
bookshelf_receiver_etc_dir = ::File.join(bookshelf_receiver_dir, 'etc')
bookshelf_receiver_log_dir = node['private_chef']['bookshelf-receiver']['log_directory']

[
  bookshelf_receiver_dir,
  bookshelf_receiver_etc_dir,
  bookshelf_receiver_log_dir
].each do |dir|
  directory dir do
    owner OmnibusHelper.new(node).ownership['owner']
    group OmnibusHelper.new(node).ownership['group']
    mode node['private_chef']['service_dir_perms']
    recursive true
  end
end

rsyncd_config = ::File.join(bookshelf_receiver_etc_dir, "rsyncd.conf")
template rsyncd_config do
  source "rsyncd.conf.erb"
  owner "root"
  owner "root"
  variables(node['private_chef']['bookshelf-receiver'])
end

directory "#{node['runit']['sv_dir']}/bookshelf-receiver"

file "#{node['runit']['sv_dir']}/bookshelf-receiver/down" do
  action :create
end

component_runit_service "bookshelf-receiver"
