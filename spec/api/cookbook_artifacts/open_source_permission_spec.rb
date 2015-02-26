# Copyright: Copyright (c) 2015 Chef Software, Inc.
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

require 'api/cookbooks_shared/open_source_permission'

describe 'Cookbook Artifacts Endpoint Open Source Permission Checks', platform: :open_source, cookbook_artifacts: true, skip: !Pedant::Config.policies? do

  let(:cookbook_url_base) { "cookbook_artifacts" }

  include_examples "Cookbook API Open Source Permissions"

end
