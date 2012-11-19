module Pedant
  module MultiTenant
    module OrgInfo
      extend Pedant::Concern

      included do
        let(:organization) { platform.test_org }
        let(:org)          { organization.name }
      end
    end
  end
end
