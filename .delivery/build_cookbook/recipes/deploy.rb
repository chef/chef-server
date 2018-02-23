#
# Cookbook Name:: build_cookbook
# Recipe:: deploy
#
# Copyright:: Copyright 2017 Chef Software, Inc.
# License:: Apache License, Version 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

infra_nodes = infra_nodes_for(workflow_change_project, workflow_change_pipeline, workflow_stage)
infra_node_names = infra_nodes.map(&:name)

# Set the run list on all nodes
infra_nodes.each do |infra_node|
  chef_node infra_node.name do
    chef_server automate_chef_server_details
    run_list %W(
      recipe[cd-infrastructure-base::default]
      recipe[chef-server-deploy::#{recipe_for(infra_node)}]
      recipe[cd-infrastructure-base::audit]
    )
  end
end

# Execute a CCR on the instances to bring up Chef Server
parallel_remote_execute "Run CCR on #{infra_node_names}" do
  command 'sudo /opt/chef/bin/chef-client --no-fork'
  hosts infra_node_names
  private_key aws_private_key
  timeout 7200 # 2 hours
end
