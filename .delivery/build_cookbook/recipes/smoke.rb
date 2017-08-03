#
# Cookbook Name:: build_cookbook
# Recipe:: smoke
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

# Trigger the Jenkins smoke tests
expeditor_jenkins_job "#{workflow_change_project}-test" do # ~FC005
  git_ref workflow_change_merge_sha
  initiated_by workflow_project_slug
  action :trigger_async
  not_if { skip_omnibus_build? }
  only_if { workflow_stage?('acceptance') }
end

# If there are every additional activities that you want to do while your
# omnibus test is happening (i.e. run inspec tests), you can do them
# here between the async trigger and the wait_for_complete.

#########################################################################
# Inspec Smoke Tests
#########################################################################

# Get a list of all the nodes that we need to test against
infra_nodes = infra_nodes_for(workflow_change_project, workflow_change_pipeline, workflow_stage)

chef_server_nodes = infra_nodes.find_all { |infra_node| infra_node['chef_product_key'] == 'chef-server' }
chef_server_fqdns = chef_server_nodes.map(&:name)

# We will run all our inspec commands in parallel, so add the profiles you want
# to execute to this array.
inspec_commands = []

# Tests to run against Chef Server instances in every environment
chef_server_smoke_tests = %w(
  chef-server-smoke
)
inspec_commands << inspec_commands_for(chef_server_smoke_tests, chef_server_fqdns, sudo: true)

# Execute all the tests in parallel (for speed!)
parallel_execute "Execute inspec smoke tests against #{workflow_stage}" do
  commands inspec_commands.flatten.uniq
  cwd workflow_workspace_repo
  environment(
    'PATH' => chefdk_path,
    'HOME' => workflow_workspace
  )
end

# Wait for the Jenkins smoke tests to complete
expeditor_jenkins_job "#{workflow_change_project}-test" do
  git_ref workflow_change_merge_sha
  initiated_by workflow_project_slug
  action :wait_until_complete
  not_if { skip_omnibus_build? }
  only_if { workflow_stage?('acceptance') }
end
