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

dependency "berkshelf2"

build do
  env = with_standard_compiler_flags(with_embedded_path)

  gem "install uuidtools " \
      " -v 2.1.3" \
      " --no-rdoc --no-ri", env: env

  command "berks install" \
          " --path=#{install_dir}/embedded/cookbooks", env: env, cwd: "#{project_dir}/private-chef"

  block do
    File.open("#{install_dir}/embedded/cookbooks/dna.json", "w") do |f|
      f.write JSON.fast_generate(
        run_list: [
          'recipe[private-chef::default]',
        ]
      )
    end
    File.open("#{install_dir}/embedded/cookbooks/show-config.json", "w") do |f|
      f.write JSON.fast_generate(
        run_list: [
          'recipe[private-chef::show_config]'
        ]
      )
    end
    File.open("#{install_dir}/embedded/cookbooks/post_upgrade_cleanup.json", "w") do |f|
      f.write JSON.fast_generate(
        run_list: [
          'recipe[private-chef::post_11_upgrade_cleanup]',
          'recipe[private-chef::post_12_upgrade_cleanup]'
        ]
      )
    end
    File.open("#{install_dir}/embedded/cookbooks/solo.rb", "w") do |f|
      f.write <<-EOH.gsub(/^ {8}/, '')
        cookbook_path   "#{install_dir}/embedded/cookbooks"
        file_cache_path "#{install_dir}/embedded/cookbooks/cache"
        verbose_logging true
        ssl_verify_mode :verify_peer
        client_fork false
      EOH
    end
  end
end
