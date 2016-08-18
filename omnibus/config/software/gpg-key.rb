#
# Author:: Tyler Cloke (<tyler@chef.io>)
# Copyright:: Copyright (c) 2012-2014 Chef Software, Inc.
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


# some versions of rhel yum does not support ssl protocol SNI, which is what
# cloudfront uses which is where the gpg key is stored. use remote_file to
# store the key locally so rhel can use it when verifying add ons
name "gpg-key"
default_version "1.0.1"

license :project_license
skip_transitive_dependency_licensing true

version "1.0.1" do
  source md5: "369efc3a19b9118cdf51c7e87a34f266"
end

source url: "https://downloads.chef.io/packages-chef-io-public.key"

build do
  mkdir "#{install_dir}/embedded/keys"
  copy "#{project_dir}/packages-chef-io-public.key", "#{install_dir}/embedded/keys/packages-chef-io-public.key"
end
