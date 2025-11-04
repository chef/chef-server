# Copyright: Copyright (c) Chef Software, Inc.
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

# Minimal implementation of the Concern pattern to replace ActiveSupport::Concern
# This provides the 'included' hook functionality and ClassMethods extension needed by Pedant modules

module Pedant
  module Concern
    def self.extended(base)
      base.instance_variable_set(:@_dependencies, [])
    end

    # This method is called in two ways:
    # 1. included do ... end - to register a block to be executed when the module is included
    # 2. included(base) - called by Ruby when the module is actually included (via super)
    def included(base = nil, &block)
      if block_given?
        # Store the block to be executed later when module is included
        @_included_block = block
      else
        # This is the actual inclusion - super will call Module#included
        super if base
      end
    end

    def append_features(base)
      # First, add the module's features (methods, constants, etc.)
      super

      # Then execute any included block in the context of the base class
      # instance_exec evaluates the block with self as base, and passes base as arg
      # This supports both: included do ... end  AND  included do |base| ... end
      if instance_variable_defined?(:@_included_block) && @_included_block
        base.instance_exec(base, &@_included_block)
      end

      # Automatically extend base with ClassMethods if it exists
      # This is the key feature of ActiveSupport::Concern
      if const_defined?(:ClassMethods)
        base.extend const_get(:ClassMethods)
      end
    end
  end
end
