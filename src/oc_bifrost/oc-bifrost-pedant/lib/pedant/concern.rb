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

# ActiveSupport::Concern is needed to help abstract out the boilerplate for creating includable modules
# TODO: Consider embedding active_support/concern directly. The difference in license will have to be addressed.

require 'active_support/concern'

module Pedant
  Concern = ActiveSupport::Concern

  # This is a trick passed on by Dan Deleo. This creates a module Pedant::Concern that
  # is exactly ActiveSupport::Concern. We can then reference Pedant::Concern instead of
  # ActiveSupport::Concern. If we want to embed ActiveSupport::Concern, we can fill out
  # Pedant::Concern without having to do a massive refactor.
end
