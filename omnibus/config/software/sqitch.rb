#
# Copyright 2014 Chef Software, Inc.
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

name "sqitch"
default_version "1.4.1"

skip_transitive_dependency_licensing true

license "MIT"
license_file "https://raw.githubusercontent.com/theory/sqitch/master/README.md"

dependency "perl"
dependency "cpanminus"

# install a LGPL-licensed version of libintl-perl:
dependency "libintl-perl"

# version_list: url=https://github.com/theory/#{name}/releases/download/v#{version}/ filter=app-sqitch-*.tar.gz
version("1.4.1")  { source sha256: "caf31cc8f772e3a4c9d4b3ff3a8f684a6eb5b1b3c261f4ddc0f90a88c36007c6" }
version("1.4.0")  { source sha256: "b0db387031f77562662e003bc55d7a102a26380b4ad7fdb9a8a3bad5769e501c" }
version("1.3.1")  { source sha256: "f5e768d298cd4047ee2ae42319782e8c2cda312737bcbdbfaf580bd47efe8b94" }
version("1.3.0")  { source sha256: "7d07635ec77a7faf3c50281c76ec833c68702f14470996cb2203a8bc6abc5bf2" }
version("1.2.1")  { source sha256: "020835a13429effd8fda12d5627604ecf99293775918f4f8ba9ccc5ed796e5e7" }
version("1.1.0")  { source sha256: "ee146cd75d6300837e6ca559bb0bde247d42123c96b2c5d4b2800f38d3e3d1ab" }
version("0.9999") { source sha256: "f5bfa80206738ab8a70358a3b0557661c7459e11ec07dece23ecafa1f34372b3" }
version("0.973")  { source sha256: "95fc7f18fff786c5d2579133e2e3ac56779e54bb3a06a1af1117054e9f49ab32" }

if version >= "1.1.0"
  source url: "https://github.com/theory/#{name}/releases/download/v#{version}/app-sqitch-v#{version}.tar.gz"
  internal_source url: "#{ENV["ARTIFACTORY_REPO_URL"]}/#{name}/#{name}-#{version}.tar.gz",
           authorization: "X-JFrog-Art-Api:#{ENV["ARTIFACTORY_TOKEN"]}"
  relative_path "App-Sqitch-v#{version}"
else
  source url: "https://github.com/theory/#{name}/releases/download/v#{version}/app-sqitch-#{version}.tar.gz"
  internal_source url: "#{ENV["ARTIFACTORY_REPO_URL"]}/#{name}/#{name}-#{version}.tar.gz",
           authorization: "X-JFrog-Art-Api:#{ENV["ARTIFACTORY_TOKEN"]}"
  relative_path "App-Sqitch-#{version}"
end

# See https://github.com/theory/sqitch for more
build do
  env = with_standard_compiler_flags(with_embedded_path)
  # Lists-MoreUtils-XS does not build on RHEL 5 or SUSE 11 currently.
  # This option is used by the Lists-MoreUtils build configuration to
  # decide whether to use the -XS package or a pure perl
  # implementation.
  env["PERL_MM_OPT"] = "PUREPERL_ONLY=1"
  command "perl Build.PL", env: env
  command "./Build installdeps --cpan_client 'cpanm -v --notest'", env: env
  command "./Build", env: env
  command "./Build install", env: env

  # Here is another licensing fun. Some of the dependencies of sqitch
  # unfortunately have GPL3 and LGPL3 licenses which are requiring us to remove
  # them from our packages after installing sqitch. Here we are uninstalling
  # them without breaking the licensing information collection.
  %w{Test-MockModule}.each do |package_name|
    module_name = package_name.gsub("-", "::")

    # Here we run cpanm --uninstall with a different PERL_CPANM_HOME. The reason
    # for this is to keep the licensing information for sqitch intact. The way
    # license_scout works is to look into PERL_CPANM_HOME/latest-build (by
    # default ~/.cpanm/latest-build) which contains the modules installed during
    # the last install. This directory is a symlink that points to the directory
    # contains the information about the latest build. Without changing
    # PERL_CPANM_HOME we would overwrite the link and will not be able to
    # collect the dependencies installed to our package while doing the actual
    # sqitch install.
    Dir.mktmpdir do |tmpdir|
      command "cpanm --force --uninstall #{module_name}", env: env.merge({
        "PERL_CPANM_HOME" => tmpdir,
      })
    end

    # Here we are removing the problematic package from the original
    # PERL_CPANM_HOME cache directory. This ensures that we do not add
    # licensing information about these components to our package.
    cpanm_root = File.expand_path("~/.cpanm/latest-build")
    delete "#{cpanm_root}/#{package_name}*"
  end

end
