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
# We use the nokogiri gem in the project,
# which pulls in libxml2.  On s390 the libxml2 lib is not built
# at the time we're building oc_id, and so fails.
# We add nokogiri as a dep because it's a direct dependency,
# and makeds more sense than adding the thing that is stopping
# it from building on s390 (libxml2).
dependency "nokogiri"
dependency "postgresql92" # for libpq
dependency "nodejs-binary"
dependency "ruby"
dependency "bundler"

relative_path "oc-id"

build do
  env = with_standard_compiler_flags(with_embedded_path)
  env['PATH'] = "#{env['PATH']}:#{install_dir}/embedded/nodejs/bin"

  bundle "config build.nokogiri --use-system-libraries" \
         " --with-xml2-config=#{install_dir}/embedded/bin/xml2-config" \
         " --with-xslt-config=#{install_dir}/embedded/bin/xslt-config"

  bundle "install" \
         " --path=#{install_dir}/embedded/service/gem" \
         " --without development test doc", env: env

  bundle "exec rake assets:precompile", env: env

  sync project_dir, "#{install_dir}/embedded/service/oc_id/", exclude: ['**/.gitignore', 'log/', 'tmp/']
end
