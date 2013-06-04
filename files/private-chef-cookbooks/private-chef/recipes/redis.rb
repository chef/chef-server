#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2011 Opscode, Inc.
# License:: Apache License, Version 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

redis_dir = node['private_chef']['redis']['dir']
redis_etc_dir = File.join(redis_dir, "etc")
redis_data_dir = File.join(redis_dir, "data")
redis_log_dir = node['private_chef']['redis']['log_directory']
[
  redis_dir,
  redis_etc_dir,
  redis_data_dir,
  redis_log_dir,
].each do |dir_name|
  directory dir_name do
    owner node['private_chef']['user']['username']
    mode '0700'
    recursive true
  end
end

redis_config = File.join(redis_etc_dir, "redis.conf")

template redis_config do
  source "redis.conf.erb"
  owner "root"
  group "root"
  mode "0644"
  variables(node['private_chef']['redis'].to_hash)
  notifies :restart, 'service[redis]' if OmnibusHelper.should_notify?("redis")
end

runit_service "redis" do
  down node['private_chef']['redis']['ha']
  options({
    :log_directory => redis_log_dir,
    :svlogd_size => node['private_chef']['redis']['log_rotation']['file_maxbytes'],
    :svlogd_num  => node['private_chef']['redis']['log_rotation']['num_to_keep']
  }.merge(params))
end

if node['private_chef']['bootstrap']['enable']
	execute "/opt/opscode/bin/private-chef-ctl start redis" do
		retries 20
	end
end
