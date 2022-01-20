#
# Copyright:: Chef Software, Inc.
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

provides :oc_id_application

property :write_to_disk, [true, false], default: false

property :redirect_uri, String, required: true

action :create do
  converge_by "create oc-id application '#{new_resource.name}'" do
    attributes = create!

    if new_resource.write_to_disk
      directory "/etc/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/oc-id-applications" do
        owner 'root'
        group 'root'
        mode '0755'
      end

      file "/etc/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/oc-id-applications/#{new_resource.name}.json" do
        content Chef::JSONCompat.to_json_pretty(attributes)
        owner 'root'
        group 'root'
        mode '0600'
      end
    end
  end
end

action_class do
  def create!
    @attributes ||= begin
                      env_helper = 'veil-env-helper --use-file -s chef-server.webui_key -s oc_id.sql_password -s oc_id.secret_key_base'
                      rails_script = <<~EOF
                              app = Doorkeeper::Application.find_or_create_by(:name => "#{new_resource.name}");
                              app.update(:redirect_uri => "#{new_resource.redirect_uri}");
                              puts app.to_json
                            EOF
                      # in order to account for rails logging, we take only the last line of output
                      # from the rails runner script. if the logging is parsed as json, we end up
                      # with a difficult-to-comprehend error message that looks like:
                      #
                      # ```
                      # Chef::Exceptions::JSON::ParseError: lexical error: invalid char in json text.
                      #                            I, [2015-05-07T18:26:37.236655
                      #          (right here) ------^
                      # ```
                      json = shell_out!("#{env_helper} -- bin/rails runner -e production '#{rails_script}'",
                        cwd: "/opt/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/embedded/service/oc_id").stdout.lines.last.chomp

                      Chef::JSONCompat.from_json(json).delete_if { |key| %w( id created_at updated_at).include? key }
                    end
  end
end
