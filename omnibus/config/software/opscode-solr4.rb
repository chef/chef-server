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

name "opscode-solr4"
default_version "4.5.1"

source url: "http://www.dsgnwrld.com/am/lucene/solr/4.5.1/solr-#{version}.tgz",
       md5: "7c8c9fbbade5c119288b06c501fa46b2"

dependency "server-jre"

relative_path "solr-#{version}"

service_dir = "#{install_dir}/embedded/service/opscode-solr4"

build do
  env = with_standard_compiler_flags(with_embedded_path)

  # copy over the licenses
  sync "licenses/", "#{service_dir}/licenses/"
  copy "LICENSE.txt", "#{service_dir}/"
  copy "NOTICE.txt", "#{service_dir}/"

  # clean up solr jetty and copy
  #
  # we'll remove all of the examples that ship with solr and build our own Solr home
  # with the chef recipes
  sync "example/", "#{service_dir}/jetty/"
  delete "#{service_dir}/jetty/example*"
  delete "#{service_dir}/jetty/multicore"
  delete "#{service_dir}/jetty/solr"
end
