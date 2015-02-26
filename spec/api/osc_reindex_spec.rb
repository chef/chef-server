require 'pedant/rspec/data_bag_util'
require 'pedant/rspec/role_util'
require 'pedant/rspec/search_util'
require 'pedant/rspec/node_util'
require 'pedant/rspec/environment_util'
require 'pedant/rspec/open_source_client_util'

describe "Server-side reindexing" do
  include Pedant::RSpec::DataBagUtil
  include Pedant::RSpec::RoleUtil
  include Pedant::RSpec::SearchUtil
  include Pedant::RSpec::NodeUtil
  include Pedant::RSpec::EnvironmentUtil

  shared(:admin_requestor){admin_user}
  shared(:requestor){admin_requestor}

  context "reindexing", :platform => :open_source, :omnibus => true do
    it_should_behave_like "Reindexing" do
      let(:executable){"/opt/chef-server/embedded/service/erchef/bin/reindex-chef-server"}
    end
  end

end
