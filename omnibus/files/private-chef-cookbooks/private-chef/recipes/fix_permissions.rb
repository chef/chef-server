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

LIB_PATH = '/opt/opscode/embedded/lib'.freeze
# The GEM_PATH should work since we allow only one version of ruby to be installed.
GEM_PATH = "#{LIB_PATH}/ruby/gems/*/gems".freeze

execute "find #{GEM_PATH} -perm /u=x,g=x,o=x -exec chmod 755 {} \\;" do
  user 'root'
end

execute "find #{GEM_PATH} -perm /u=r,g=r,o=r ! -perm /u=x -exec chmod 644 {} \\;" do
  user 'root'
end
