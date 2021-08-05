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

open_source_11_sv_dir = '/opt/chef-server/sv'

return unless Dir.exist?(open_source_11_sv_dir)

# find all of the directory entries in the sv_dir that are
# not hidden, '..', or '.'.
services = Dir.new(open_source_11_sv_dir).entries.select do |d|
  File.directory?(File.expand_path(d, open_source_11_sv_dir)) &&
    !/\..*/.match(d)
end

services.each do |sv_entry|
  sv_dir = File.expand_path(sv_entry, open_source_11_sv_dir)
  down_file = File.expand_path('down', sv_dir)
  file down_file do
    owner 'root'
    group 'root'
    mode '644'
    content ''
  end
end
