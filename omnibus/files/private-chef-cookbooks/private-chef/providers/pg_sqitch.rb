#
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

action :deploy do
  target = new_resource.target_version ? "--to-target #{new_resource.target_version}" : ""
  converge_by "Deploying schema from #{new_resource.name}" do
    execute "sqitch_deploy_#{new_resource.name}" do
      command <<-EOM.gsub(/\s+/," ").strip!
        sqitch --engine pg
               --db-name #{new_resource.database}
               --db-host #{new_resource.hostname}
               --db-port #{new_resource.port}
               --db-user #{new_resource.username}
               --top-dir #{new_resource.name}
               deploy #{target} --verify
      EOM
      environment "PERL5LIB" => "", # force us to use omnibus perl
                  "PGPASSWORD" => new_resource.password,
                  "PGSSLMODE" => node['private_chef']['postgresql']['sslmode']

      # Sqitch Return Codes
      # 0 - when changes are applied
      # 1 - when everything is ok but no changes were made
      # 2(+?) - when an error occurs.
      returns [0,1]
    end
  end
end
