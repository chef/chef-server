#
# Copyright 2012-2014 Chef Software, Inc.
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
default_version "2.0.5"

dependency "pg-gem"
dependency "sequel-gem"

build do
  env = with_standard_compiler_flags(with_embedded_path)

  # Ignore dependencies, since they are already installed on the system by
  # omnibus from one of the dependencies. This ensures we can properly link the
  # pg to the needed headers without needing to pass options through
  # knife-ec-backup.
  gem "install knife-ec-backup" \
      " --version '#{version}'" \
      " --bindir '#{install_dir}/embedded/bin'" \
      " --ignore-dependencies" \
      " --no-ri --no-rdoc", env: env
end
