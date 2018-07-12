#
# Copyright:: 2015-2018 Chef Software, Inc.
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
# PostgreSQL Cleanup from EC 1.4 and earlier
#

# Prior to Private Chef 11, the PostgreSQL service was named
# 'postgres', not 'postgresql'.  We need to cleanup all traces of the
# previously-named service if it is present.

runit_service "postgres" do
  action [:stop, :disable]
end

# I wish runit_service had a :destroy action...
directory "/opt/opscode/sv/postgres" do
  action :delete
  recursive true
end

#
# Bootstrap Service Cleanup from Chef Server 12.3.1 and earlier.
#

# Bootstrap is now part of the private_chef cookbook itself and is
# no longer an additional component - remove any traces of it.
directory "/opt/opscode/embedded/service/chef-server-bootstrap" do
  action :delete
  recursive true
  ignore_failure true
end

#
# Opscode Expander Reindexer from Chef Server 12.3.1 and earlier
#

# opscode-expander-reindexer was removed in 56c156bc71201dc8bf921ef69cfff4db3e9ff898
runit_service "opscode-expander-reindexer" do
  action [:stop, :disable]
end

link "/opt/opscode/init/opscode-expander-reindexer" do
  action :delete
end

directory "/opt/opscode/sv/opscode-expander-reindexer" do
  action :delete
  recursive true
end
