#
# Copyright:: Copyright (c) 2015 Chef Software, Inc.
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
require_relative 'preflight_checks'

class BookshelfPreflightValidator < PreflightValidator
  attr_reader :user_attrs, :node_attrs

  def initialize(node)
    super
    @user_attrs = PrivateChef['bookshelf']
    @node_attrs = node['private_chef']['bookshelf']
  end

  def run!
    verify_storage_type_unchanged
  end

  # 5 Cases
  # (1) First install, anything goes => OK
  # (2) An old storage type was not set and the new_storage_type is :filesystem => OK
  # (3) An old storage type was not set and the new_storage_type is :sql => NOT_OK
  # (4) An old storage type was set and is not equal to the new storage type => NOT_OK
  # (5) An old storage type was set and is equal to the new storage type => OK
  def verify_storage_type_unchanged
    if previous_run.nil? # case (1)
      true
    else
      previous_value = previous_run['bookshelf']['storage_type']
      current_value = user_attrs['storage_type'] || 'filesystem'

      if previous_value.nil? && current_value == 'filesystem' # case (2)
        true
      elsif previous_value.nil? && current_value != "filesystem" # case (3)
        fail_with <<EOM

Bookshelf's storage_type was previously the default of 'filesystem';
however the current configuration would result in a value of
'#{current_value}'. At this time it is not possible to change the
bookshelf storage_type post-installation.

Please set

bookshelf['storage_type'] = 'filesystem'

in /etc/opscode/chef-server.rb or leave it unset.
EOM
      elsif previous_value == current_value # case (5)
        true
      else # everything else is invalid, including case 4 above
        fail_with <<EOM

Bookshelf's storage_type was previously '#{previous_value}'; however
the current configuration would result in a value of '#{current_value}'.
At this time it is not possible to change the bookshelf storage_type post-installation.

Please set

bookshelf['storage_type'] = '#{previous_value}'

in /etc/opscode/chef-server.rb
EOM

      end
    end
  end
end
