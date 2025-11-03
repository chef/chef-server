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
# This provides the 'included' hook functionality needed by Pedant modules

module Pedant
  module Concern
    def self.extended(base)
      base.instance_variable_set(:@_dependencies, [])
    end

    def included(base = nil, &block)
      if base.nil?
        raise ArgumentError, "Missing argument: `included' expects a block or a base module" unless block_given?

        @_included_block = block
      else
        super
      end
    end

    def append_features(base)
      if instance_variable_defined?(:@_included_block)
        base.class_eval(&@_included_block)
      end
      super
    end
  end
end
