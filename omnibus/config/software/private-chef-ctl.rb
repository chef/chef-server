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

name "chef-server-ctl"

source path: "#{project.files_path}/../../src/chef-server-ctl"

license :project_license
skip_transitive_dependency_licensing true

dependency "postgresql96" # for libpq
dependency "omnibus-ctl"
# Used by `chef-server-ctl install` to resolve download urls
dependency "mixlib-install"

build do

  env = with_standard_compiler_flags(with_embedded_path)

  # we would like to do this '--without development' but appbundler is insisting
  # on installing
  bundle "install", env: env

  gem "build chef-server-ctl.gemspec", env: env

  # Hack: install binaries in /tmp because we don't actually want them at all
  gem "install chef-server-ctl-*.gem --no-ri --no-document --verbose --bindir=/tmp", env: env

  appbundle "chef-server-ctl", env: env

  link "#{install_dir}/bin/chef-server-ctl", "#{install_dir}/bin/private-chef-ctl"

  # These are necessary until we remove all hardcoded references to embedded/bin/*-ctl
  link "#{install_dir}/bin/chef-server-ctl", "#{install_dir}/embedded/bin/chef-server-ctl"
  link "#{install_dir}/bin/chef-server-ctl", "#{install_dir}/embedded/bin/private-chef-ctl"
end
