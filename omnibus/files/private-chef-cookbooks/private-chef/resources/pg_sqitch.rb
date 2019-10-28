#
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
default_action :deploy

attribute :name,           kind_of: String, required: true, name_attribute: true
attribute :database,       kind_of: String, required: true
attribute :username,       kind_of: String
attribute :password,       kind_of: String, required: false, default: ''
attribute :target_version, kind_of: String
attribute :hostname,       kind_of: String, required: true
attribute :port,           kind_of: Integer, required: true
attribute :sslmode,        kind_of: String, required: false, default: 'disable'
