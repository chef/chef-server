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
build_version   IO.read(File.expand_path("../../../../VERSION", __FILE__)).strip
build_iteration 1

# In order to prevent unecessary cache expiration,
# package and package version overrides, build_version
# and build_iteration are kept in <project-root>/omnibus_overrides.rb
overrides_path = File.expand_path("../../../../omnibus_overrides.rb", __FILE__)
instance_eval(IO.read(overrides_path), overrides_path)

# creates required build directories
dependency "preparation"

# Meta-dependency containing all of
# the Chef Server dependencies.
dependency "server-complete"

# THESE MUST BE LAST DO NOT MOVE
dependency "ruby-cleanup"
dependency "cleanup"

package :rpm do
  signing_passphrase ENV['OMNIBUS_RPM_SIGNING_PASSPHRASE']
  compression_level 6
  compression_type :xz
end

package :deb do
  compression_level 6
  compression_type :xz
end

exclude "**/.git"
exclude "**/bundler/git"
