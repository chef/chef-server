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
default_version "4.10.4"

license "Apache-2.0"
license_file "LICENSE.txt"
skip_transitive_dependency_licensing true

source url: "http://archive.apache.org/dist/lucene/solr/#{version}/solr-#{version}.tgz",
       md5: "8ae107a760b3fc1ec7358a303886ca06"


if ppc64? || ppc64le? || s390x?
  dependency "ibm-jre"
elsif intel? && _64_bit?
  dependency "server-jre"
elsif armhf?
  dependency "jre-from-jdk"
else
  raise "A JRE is required by opscode-solr4, but none are known for this platform"
end

relative_path "solr-#{version}"

service_dir = "#{install_dir}/embedded/service/opscode-solr4"

build do
  env = with_standard_compiler_flags(with_embedded_path)

  embedded_jre_path = "#{install_dir}/embedded/jre/bin"

  unless @ibm_jre
    env['PATH'] = "#{env['PATH']}:#{embedded_jre_path}"
  end

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

  # Replace the built-in admin.html with a blank page to effectively disable the admin
  # interface to solr.  Note that this does not disable API access.
  touch "admin.html"

  #
  # Locate an executable in the current $PATH.
  #
  # @param [String] executable
  #   path or name of the executable you are looking for
  #
  # @param [Hash] env
  #   an environment with a PATH that you would like to search
  #
  # @return [String, nil]
  #   the path to the executable, or +nil+ if not present
  #
  def which(executable, env=ENV)
    if File.file?(executable) && File.executable?(executable)
      executable
    elsif env["PATH"]
      path = env["PATH"].split(File::PATH_SEPARATOR).find do |path|
        File.executable?(File.join(path, executable))
      end

      path && File.expand_path(executable, path)
    end
  end

  if which("jar", env)
    command "jar -uf #{service_dir}/jetty/webapps/solr.war admin.html", env: env
  elsif which("zip", env)
    command "zip  #{service_dir}/jetty/webapps/solr.war admin.html", env: env
  else
    raise "Builder does not have a zip or jar executable"
  end

  delete "admin.html"
end
