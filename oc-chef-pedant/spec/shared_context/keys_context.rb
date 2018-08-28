# Copyright: Copyright 2015-2018 Chef Software, Inc.
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

def rand_id
  rand(10**7...10**8).to_s
end

RSpec.shared_context 'when the current_requestor is the superuser' do
  let(:current_requestor) { superuser }
end

RSpec.shared_context 'when the current_requestor is a user in the main org' do
  let(:test_user_name) { "pedant_test_user_#{rand_id}" }
  let(:current_requestor) { platform.create_user(test_user_name) }

  before do
    platform.associate_user_with_org(org_name, current_requestor)
  end

  after do
    platform.delete_user(current_requestor)
  end
end

RSpec.shared_context 'when the current_requestor is a client in the main org' do
  let(:test_client_name) { "pedant_test_client_#{rand_id}" }
  let(:current_requestor) { platform.create_client(test_client_name, @test_org) }

  after do
    platform.delete_client(current_requestor, @test_org)
  end
end

RSpec.shared_context 'when the current_requestor is a user in a different org' do
  let(:test_user_name) { "pedant_test_user_#{rand_id}" }
  let(:test_org_name) { "pedant_test_org_#{rand_id}" }
  let(:current_requestor) { platform.create_user(test_user_name) }
  let(:second_org) { platform.create_org(test_org_name) }

  before do
    second_org
    platform.associate_user_with_org(test_org_name, current_requestor)
  end

  after do
    platform.delete_org(test_org_name)
    platform.delete_user(current_requestor)
  end
end

RSpec.shared_context 'when the current_requestor is a client in a different org' do
  let(:test_client_name) { "pedant_test_client_#{rand_id}" }
  let(:test_org_name) { "pedant_test_org_#{rand_id}" }
  let(:second_org) { platform.create_org(test_org_name) }
  let(:current_requestor) { platform.create_client(test_client_name, second_org) }

  after do
    platform.delete_client(current_requestor, second_org)
    platform.delete_org(test_org_name)
  end
end
