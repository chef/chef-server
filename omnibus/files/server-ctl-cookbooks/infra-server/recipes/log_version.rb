#
# Copyright:: Chef Software, Inc.
# Author:: Sudheer ()
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

chef_server_version_history = '/var/opt/opscode/chef_version_history.txt'

file chef_server_version_history do
  mode '0755'
  action :create_if_missing
end

bash 'append_to_config' do
  code <<-EOF
    date1=`date`
    version_line=`grep 'chef-server ' /opt/opscode/version-manifest.txt`
    version=`echo "$version_line" | cut -d' ' -f2`
    grep -qw "$version" < "#{chef_server_version_history}"
    ret_val=$?
    if [[ ${ret_val} -ne 0 ]]
    then
      echo "$date1: $version_line" >> "#{chef_server_version_history}"
    fi
  EOF
end
