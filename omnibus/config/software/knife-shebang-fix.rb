#
# Copyright 2012-2020, Chef Software Inc.
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

#
# Use this software definition to fix the shebang of knife binary
# to point to the embedded ruby.
#

name "knife-shebang-fix"

default_version "0.0.1"

license :project_license
skip_transitive_dependency_licensing true

build do
  if windows?
    # No action needed on Windows
  else
    block "Update knife shebang to point to embedded Ruby" do
      # Fix the shebang for knife binary specifically
      # #!/usr/bin/env ruby
      knife_file = "#{install_dir}/embedded/bin/knife"
      if File.exist?(knife_file)
        update_shebang = false
        rest_of_the_file = ""

        File.open(knife_file) do |f|
          shebang = f.readline
          if shebang.start_with?("#!") &&
              shebang.include?("ruby") &&
              !shebang.include?("#{install_dir}/embedded/bin/ruby")
            rest_of_the_file = f.read
            update_shebang = true
          end
        end

        if update_shebang
          File.open(knife_file, "w+") do |f|
            f.puts("#!#{install_dir}/embedded/bin/ruby")
            f.puts(rest_of_the_file)
          end
        end
      end
    end
  end
end
