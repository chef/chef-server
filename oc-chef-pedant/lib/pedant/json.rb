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

require 'json'

module Pedant
  module JSON
    def parse(json_string)
      ::JSON.parse(json_string,
                 :create_additions => false,
                 # some keys don't play nicely as Ruby symbols (embedded hyphens, e.g.)
                 :symbolize_names => false)
    end

    def to_json(data)
      ::JSON.generate(data)
    end
  end
end
