#
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

name "knife-ec-backup"
# git shaw from June 18th, 2014. Latest at time of this coding
default_version "6f721811234699838f4920720aea1eea05ba32db"

dependency "rsync"

source :git => "git@github.com:opscode/knife-ec-backup.git"

# put knife-ec-backup on the system but don't install it.
# It will need to be built and installed at runtime during an upgrade
# from OSC to ensure we can link against the OSC postgres install
build do
  command "mkdir -p #{install_dir}/embedded/lib/knife-ec-backup"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/lib/knife-ec-backup"
end
