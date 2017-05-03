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

name "chef-server"
maintainer "Chef Software, Inc. <maintainers@chef.io>"
homepage   "https://www.chef.io"
license "Apache-2.0"
license_file "LICENSE"

package_name    "chef-server-core"
replace         "private-chef"
conflict        "private-chef"
install_dir     "/opt/opscode"
build_version   "12.15.2"
build_iteration 1
#
# Load dynamically updated overrides
overrides_path = File.expand_path("../../../../omnibus_overrides.rb", __FILE__)
instance_eval(IO.read(overrides_path), overrides_path)

# creates required build directories
dependency "preparation"

dependency "server-complete"

dependency "cleanup" # MUST BE LAST DO NOT MOVE

# if this is a release build, use a higher compression level
xz_level = ENV['PIPELINE_TRIGGER_JOB_NAME'] == 'chef-server-12-trigger-release' ? 6 : 1

package :rpm do
  signing_passphrase ENV['OMNIBUS_RPM_SIGNING_PASSPHRASE']
  compression_level xz_level
  compression_type :xz
end

package :deb do
  compression_level xz_level
  compression_type :xz
end

exclude "**/.git"
exclude "**/bundler/git"
