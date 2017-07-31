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
#     http://www.apache.org/licenses/LICENSE-2.0
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

# Wait for the Jenkins smoke tests to complete
expeditor_jenkins_job "#{workflow_change_project}-test" do
  git_ref workflow_change_merge_sha
  initiated_by workflow_project_slug
  action :wait_until_complete
  not_if { skip_omnibus_build? }
  only_if { workflow_stage?('acceptance') }
end
