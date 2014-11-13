#
# Author:: Tyler Cloke (<tyler@getchef.com>)
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
default_version "1.0.0"

version "1.0.0" do
  source md5: "c8f49b137b190707a0c5f5702a147155"
end

source url: "https://downloads.getchef.com/chef.gpg.key"

build do
  mkdir "#{install_dir}/embedded/keys"
  copy "#{project_dir}/chef.gpg.key", "#{install_dir}/embedded/keys/chef.gpg.key"
end
