# -*- coding: utf-8 -*-
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

require 'api/cookbooks_shared/update'

# FIXME For /cookbooks/NAME/VERSION tests we have a limited checking
# on the GET afterwards to do validation of data on the server.  Since
# we can't match the payload against one with dynamically generated
# URLs we're only checking for the return code.
#
# We need to come back (along with adding tests for the GET case
# explicitly in read_spec.rb) and update the tests marked with TODO
# to actually match on the generate response body as well
#

describe "Cookbook Artifacts API endpoint", cookbook_artifacts: true, skip: !Pedant::Config.policies? do

  let(:cookbook_url_base) { "cookbook_artifacts" }

  include_examples "Cookbook Update"

end
