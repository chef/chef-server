# Copyright: Copyright 2012-2018 Chef Software, Inc.
# License: Apache License, Version 2.0
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

# Utility methods for manipulating Pedant test gems.
module Pedant
  class Gem
    class << self
      # Return an array of names of all chef-pedant gems (names end with "-pedant")
      def names
        @names ||= ::Gem::Specification.map(&:name).grep(/-pedant$/)
      end

      # Returns an array of absolute paths to Pedant gems
      def base_directories
        @base_dirs ||= names.map { |name| ::Gem::Specification.find_by_name(name).gem_dir }
      end

      # Return the absolute path to a given +relative_path+ within all
      # Pedant gems, if such a path exists.
      def absolute_paths_for(relative_path)
        base_directories.inject([]) do |acc, dir|
          f = "#{dir}/#{relative_path}"
          File.exists?(f) ? acc << f : acc
        end
      end

      # Returns an array of absolute paths to spec directories in Pedant test gems
      #
      # +suite+ is a String denoting which spec sub-directories should be accepted.
      #
      # e.g, if you wish to run tests from "/spec/api", +suite+ should
      # be "api".  This is intended to allow multiple Pedant
      # derivatives to use the same core gem, but not have to deal
      # with loading of test specs that will never be run.
      def test_directories(suite)
        absolute_paths_for("spec/#{suite}")
      end

      # Returns an array of absolute paths to fixtures directories in Pedant test gems
      def fixture_directories
        @_fixture_directories ||= absolute_paths_for('fixtures')
      end
    end
  end
end
