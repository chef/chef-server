#
# Copyright 2012-2014 Chef Software, Inc.
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

name "chef-ha-plugin-config"
description "generates chef-server-plugin.rb"
default_version "0.0.1"

license :project_license
skip_transitive_dependency_licensing true

build do
  block do
    File.open("#{install_dir}/chef-server-plugin.rb", "w") do |f|
      f.puts <<EOF
plugin "chef-ha-drbd" do
  enabled_by_default false
  cookbook_path "/opt/opscode/embedded/cookbooks"
end
EOF
    end
  end
end
