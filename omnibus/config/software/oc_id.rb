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

dependency "postgresql92" # for libpq
dependency "nodejs"
dependency "ruby"
dependency "bundler"

relative_path "oc-id"

build do
  env = with_standard_compiler_flags(with_embedded_path)

  # Tell nokogiri to use the system libraries instead of compiling its own
  env["NOKOGIRI_USE_SYSTEM_LIBRARIES"] = "true"

  bundle "config build.nokogiri" \
         " --use-system-libraries" \
         " --with-xml2-lib=#{install_dir}/embedded/lib" \
         " --with-xml2-include=#{install_dir}/embedded/include/libxml2" \
         " --with-xslt-lib=#{install_dir}/embedded/lib" \
         " --with-xslt-include=#{install_dir}/embedded/include/libxslt" \
         " --with-iconv-dir=#{install_dir}/embedded" \
         " --with-zlib-dir=#{install_dir}/embedded", env: env

  bundle "install" \
         " --path=#{install_dir}/embedded/service/gem" \
         " --without development test doc", env: env

  bundle "exec rake assets:precompile", env: env

  sync project_dir, "#{install_dir}/embedded/service/oc_id/", exclude: ['**/.gitignore', 'log/', 'tmp/']
end
