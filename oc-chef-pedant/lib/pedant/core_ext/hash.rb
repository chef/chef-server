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

module Pedant
  module CoreExt
    module Hash

      # Stores value into the hash and returns the hash
      def with!(key, value = {})
        self.tap { |h| h.store(key, value) }
      end

      # Clones hash, stores value, and returns new hash
      def with(key, value = {})
        self.dup.with!(key, value)
      end

      def except!(key)
        self.tap { |h| h.delete(key) }
      end

      # standard_request.except(:headers)
      # standard_resource.except(:json_class)
      def except(key)
        self.dup.except!(key)
      end
    end
  end
end

Hash.send(:include, Pedant::CoreExt::Hash)
