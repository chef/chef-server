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

    # Evaluate given block in context of base class, so that you can write class macros here.
    # When base is nil, stores the block to be executed later when the module is included.
    # When base is provided, calls super to maintain Ruby's module inclusion chain.
    def included(base = nil, &block)
      if base.nil?
        @_included_block = block
      else
        super
      end
    end

    def append_features(base)
      super
      base.class_eval(&@_included_block) if instance_variable_defined?(:@_included_block)
      base.extend const_get(:ClassMethods) if const_defined?(:ClassMethods)
    end
  end
end
