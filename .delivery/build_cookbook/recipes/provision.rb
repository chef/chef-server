#
# Cookbook Name:: build_cookbook
# Recipe:: provision
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

# Create an Application Release object in Chef Automate
expeditor_workflow_release workflow_change_project do
  version ::File.read(::File.join(workflow_workspace_repo, 'VERSION')).strip
  action :create
  only_if { workflow_stage?('acceptance') }
end

# Let delivery-truck do the workflow release promotion
include_recipe 'delivery-truck::provision'

# Stand up the Supermarket instance
execute 'apply-terraform' do
  command 'make apply'
  cwd repo_terraform_directory
  environment default_terraform_environment_vars
  live_stream true
end
