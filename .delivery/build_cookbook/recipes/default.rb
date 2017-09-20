#
# Cookbook Name:: build_cookbook
# Recipe:: default
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

# NOTE(ssd) 2017-09-20: Keyservers are not very reliable. Automate has
# more luck with MIT, but it fails constantly for me so I'm changing
# it here.  We have to use override because it is set explicitly in
# the expeditor-build cookbook at the normal level.
node.override['chef-apt-docker']['keyserver'] = 'na.pool.sks-keyservers.net'
include_recipe 'expeditor-build::default'
