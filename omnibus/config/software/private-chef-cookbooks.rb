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

name "private-chef-cookbooks"

source path: "#{project.files_path}/#{name}"

license :project_license

dependency "berkshelf-no-depselector"

build do
  env = with_standard_compiler_flags(with_embedded_path)

  gem "install uuidtools " \
      " -v 2.1.3" \
      " --no-document", env: env

  command "berks vendor #{install_dir}/embedded/cookbooks", env: env

  block do
    File.open("#{install_dir}/embedded/cookbooks/dna.json", "w") do |f|
      run_list = Array.new.tap do |r|
        r << 'recipe[private-chef::default]'
      end

      f.write FFI_Yajl::Encoder.encode(
        run_list: run_list
      )
    end
    File.open("#{install_dir}/embedded/cookbooks/show-config.json", "w") do |f|
      f.write FFI_Yajl::Encoder.encode(
        run_list: [
          'recipe[private-chef::show_config]'
        ]
      )
    end
    File.open("#{install_dir}/embedded/cookbooks/check-config.json", "w") do |f|
      f.write FFI_Yajl::Encoder.encode(
        run_list: [
          'recipe[private-chef::plugin_discovery]',
          'recipe[private-chef::plugin_config_extensions]',
          'recipe[private-chef::config]'
        ]
      )
    end
    File.open("#{install_dir}/embedded/cookbooks/post_upgrade_cleanup.json", "w") do |f|
      f.write FFI_Yajl::Encoder.encode(
        run_list: [
          'recipe[private-chef::post_11_upgrade_cleanup]',
          'recipe[private-chef::post_12_upgrade_cleanup]',
          'recipe[private-chef::solr4_gclog_cleanup]',
          'recipe[private-chef::postgres_upgrade_cleanup]'
        ]
      )
    end
    File.open("#{install_dir}/embedded/cookbooks/solo.rb", "w") do |f|
      f.write <<-EOH.gsub(/^ {8}/, '')
        cookbook_path   "#{install_dir}/embedded/cookbooks"
        cache_path "/var/opt/opscode/local-mode-cache"
        file_cache_path "/var/opt/opscode/local-mode-cache"
        verbose_logging true
        log_level :fatal
        ssl_verify_mode :verify_peer
        client_fork false
      EOH
    end
  end
end
