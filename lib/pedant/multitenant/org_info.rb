module Pedant
  module MultiTenant
    module OrgInfo
      extend Pedant::Concern

      included do
        let(:organization) { platform.test_org }
        let(:org)          { organization.name }
        # Ensure we are not using a bad_client in the place of a bad_user
        # see https://github.com/opscode/chef-pedant/commit/808d8c633bb94f0b13b1e7c8236ceb40562cba53
        shared(:outside_user) { platform.bad_user }
      end
    end
  end
end
