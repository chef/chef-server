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

name "private-chef-ctl"

source path: "#{project.files_path}/private-chef-ctl-commands"

license :project_license
skip_transitive_dependency_licensing true

dependency "highline-gem"
dependency "sequel-gem"
dependency "omnibus-ctl"
# TODO
# chef-server-ctl server-admins commands dep, will be removed in server-admins V2
# https://gist.github.com/tylercloke/a8d4bc1b915b958ac160#version-2
dependency "rest-client-gem"
# Used by `chef-server-ctl install` to resolve download urls
dependency "mixlib-install"

build do
  block do
    open("#{install_dir}/bin/private-chef-ctl", "w") do |file|
      file.print <<-EOH
#!/bin/bash
#
# Copyright 2012-2015 Chef Software, Inc.
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

export SVWAIT=30

# Ensure the calling environment (disapproval look Bundler) does not infect our
# Ruby environment if private-chef-ctl is called from a Ruby script.
unset RUBYOPT
unset BUNDLE_BIN_PATH
unset BUNDLE_GEMFILE
unset GEM_PATH
unset GEM_HOME

ID=`id -u`
if [ $ID -ne 0 ]; then
   echo "This command must be run as root."
   exit 1
fi

#{install_dir}/embedded/service/omnibus-ctl/chef-server-ctl opscode #{install_dir}/embedded/service/omnibus-ctl "$@"
       EOH
    end
  end

  command "chmod 755 #{install_dir}/bin/#{name}"
  link "#{install_dir}/bin/#{name}", "#{install_dir}/bin/chef-server-ctl"

  # additional omnibus-ctl commands
  sync project_dir, "#{install_dir}/embedded/service/omnibus-ctl/"
end
