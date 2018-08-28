# Copyright: Copyright 2012-2018 Chef Software, Inc.
# License: Apache License, Version 2.0
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

module Pedant
  module ChefUtility
    require 'erubis'
    require 'pathname'

    # Generate a knife.rb file from a template for a given user.
    # Prefer calling +populate_dot_chef+ over calling this directly.
    def self.generate_knife(user_name, server_url, key_dir, destination)
      # The template file is currently located right next to this
      # source file... seemed like the sanest place for it at the time
      template = File.read(Pathname.new(__FILE__).dirname.join("knife.rb.erb"))
      template = Erubis::Eruby.new(template)
      File.open(destination, 'w') do |f|
        f.write(template.result(knife_user: user_name,
                                key_dir:    key_dir,
                                server_url: server_url))
      end
    end

    # Takes a Pedant::Requestor object and a path to a directory and
    # writes that Requestor's key out as a PEM file.  The name of the
    # PEM file will be "USER_NAME.pem".
    def self.write_user_pem(user, key_dir)
      File.open("#{key_dir}/#{user.name}.pem", 'w') {|f| f.write(user.signing_key)}
    end

    # Given a Pedant::Requestor creates a knife.rb file for that Requestor,
    # along with the associated PEM  files.
    #
    # +server_url+ becomes +chef_server_url+ in the generated knife.rb
    # file.  +key_dir+ is where the client_key and validator_key are
    # generated.  +knife_destination+ is complete path to the knife.rb
    # file, including the filename (e.g. /foo/bar/knife.rb).  This
    # allows for us to create multiple knife.rb files for different
    # users / testing scenarios.
    def self.populate_dot_chef(user, server_url, dot_chef_dir, knife_rb_file_name="knife.rb")
      self.generate_knife(user.name, server_url, dot_chef_dir, "#{dot_chef_dir}/#{knife_rb_file_name}")
      self.write_user_pem(user, dot_chef_dir)
    end
  end
end
