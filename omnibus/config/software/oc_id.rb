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

name "oc_id"

source path: "#{project.files_path}/../../src/oc-id"

license "Apache-2.0"
license_file "LICENSE"

dependency "postgresql92" # for libpq
dependency "nodejs"
dependency "ruby"
dependency "bundler"

skip_transitive_dependency_licensing true

relative_path "oc-id"

build do
  env = with_standard_compiler_flags(with_embedded_path)

  bundle "config build.nokogiri --use-system-libraries" \
         " --with-xml2-config=#{install_dir}/embedded/bin/xml2-config" \
         " --with-xslt-config=#{install_dir}/embedded/bin/xslt-config"

  bundle "install" \
         " --path=#{install_dir}/embedded/service/gem" \
         " --without development test doc", env: env

  bundle "exec rake assets:precompile", env: env

  sync project_dir, "#{install_dir}/embedded/service/oc_id/", exclude: ['**/.gitignore', 'log/', 'tmp/']
end
