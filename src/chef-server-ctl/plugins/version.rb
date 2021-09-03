#
# Copyright:: Copyright (c) 2018 Chef Software, Inc.
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
require "json"

add_command_under_category "version", "general", "Display current version of the Chef Infra Server.", 2 do

  # detect if running as a habitat service
  #
  # TODO(ssd) 2018-08-09: I'm not sure what to do about this
  # one. The version isn't really configuration but this output
  # isn't appropriate in all cases.  One option would be to ask the
  # LB for our version, but then the version command won't work when
  # we are offline.

  if File.exist?("/hab/svc/chef-server-ctl/PID")
    ident_file = File.read("../IDENT")
    version = "chef-server #{ident_file.split("/")[2]}"
  else
    version = JSON.parse(File.read("/opt/opscode/version-manifest.json"))["build_version"]
  end

  puts version
rescue Errno::ENOENT => e
  puts "Error determining version!"
  puts "#{e.message}"
  exit(1)

end
