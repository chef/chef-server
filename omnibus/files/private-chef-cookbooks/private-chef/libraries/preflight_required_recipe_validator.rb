#
# Copyright:: Copyright (c) 2017 Chef Software, Inc.
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

require_relative './preflight_checks.rb'

class RequiredRecipePreflightValidator < PreflightValidator
  def initialize(node)
    super
    @required_recipe = PrivateChef['required_recipe']
  end

  def run!
    return unless @required_recipe['enable']

    verify_required_recipe_exists
    verify_required_recipe_owner
    verify_required_recipe_group
    verify_required_recipe_mode
  end

  def stat
    @stat ||= ::File.stat(@required_recipe['path'])
  end

  def verify_required_recipe_exists
    unless ::File.exist?(@required_recipe['path'])
      fail_with <<EOF
Server enforced required recipe is enabled but the recipe file does not exist or
is misconfigured. Please set the `required_recipe["path"] = /path/to/recipe` in
`/etc/opscode/chef-server.rb` and run:

    chef-server-ctl reconfigure
EOF
    end
  end

  def verify_required_recipe_owner
    unless stat.uid == 0
      fail_with <<EOF
The required_recipe file must be owned by root. Please change the owner to root
and reconfigure the Chef server:

    chown root:root #{@required_recipe['path']}
    chef-server-ctl reconfigure
EOF
    end
  end

  def verify_required_recipe_group
    unless stat.gid == 0
      fail_with <<EOF
The required_recipe file must be in the root group. Please change the group to
root and reconfigure the Chef server:

    chown root:root #{@required_recipe['path']}
    chef-server-ctl reconfigure
EOF
    end
  end

  def verify_required_recipe_mode
    unless %w(600 644).include?(format('%o', stat.mode)[3..-1])
      fail_with <<"EOF"
The required_recipe must have a mode of 644 or 600. Please set a compatible mode
and reconfigure the Chef server:

    chmod 600 #{@required_recipe['path']}
    chef-server-ctl reconfigure
EOF
    end
  end
end
