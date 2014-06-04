require 'spec_helper.rb'
include SpecHelper

# Special case: clients goes to erchef. we have to put this
# in internal-account because webui1 may not know better than
# to continue sending 'clients' requests over to int-acct
erchef_uris = %w{
  /organizations/testorg/clients
  /organizations/testorg/clients/
  /organizations/testorg/clients/avalue
  /organizations/testorg/clients/avalue/
  /organizations/testorg/clients/avalue/subvalue
  /organizations/testorg/clients/B@dIdentifier
  /organizations/testorg/clients/myclient/_aclfakeout
  /organizations/testorg/clients/_aclfake
}
# In this list will also be invalid urls - but account internal
# will take everything, sending only 'clients' endpoints over to erchef.
acct_uris = [
  "/authenticate_user",
  "/verify_password",
  "/users",
  "/users/good",
  "/users/bOrg",
  "/users/borg/association_requests",
  "/users/borg/association_requests/",
  "/users/borg/association_requests/abc",
  "/users/borg///association_requests/abc",
  "/organizations/testorg/principals",
  "/organization",
  "/organizations/TestOrg",
  "/organizations/TestOrg/",
  "/organizations/TestOrg/abc",
  "/bad/path1",
  "/",
  "/authenticate_user/",
  "/authenticate_user/bad",
  "/verify_password/",
  "/verify_password/bad",
  "/users/borg//association_requests0abc",
  "/users/borg/association_requests0abc",
  "/organizations/myorg/clients/myclient/_acl",
  "/organizations/myorg/clients/myclient/_acl/",
  "/organizations/myorg/clients/myclient/_acl/grant",
  "/organizations/myorg/clients/myclient/_acl/invalidbutaccepted",
  # Precreated org endpoints
  "/internal-organization",
  "/internal-organizations",
  "/internal-organizations/",
  "/internal-organizations/test"
]
# TODO - these will begin causing failures as components get migrated
# over to/defaulted to erchef
%w{groups users containers association_requests}.each do |endpoint|
  acct_uris << "/organizations/testorg/#{endpoint}"
  acct_uris << "/organizations/testorg/#{endpoint}/"
  acct_uris << "/organizations/testorg/#{endpoint}/valid_identifier"
  acct_uris << "/organizations/testorg/#{endpoint}/valid_identifier/subidentifier"
  acct_uris << "/organizations/testorg/#{endpoint}/BadI!dentifier"
end
acct_uris << "/organizations/testorg/users/someuser/assocation_requests"
acct_uris << "/organizations/testorg/users/someuser/assocation_requests09"

# _acl always goes to account no matter what comes before it.
# Migration Stage 3: this will be changed to erchef after an interim
# period of acct-erchef
%w{cookbooks data roles sandboxes environments
   clients nodes reports}.each do |endpoint|
  acct_uris << "/organizations/testorg/#{endpoint}/someidentifier/_acl"
  acct_uris << "/organizations/testorg/#{endpoint}/_acl"
  acct_uris << "/organizations/testorg/#{endpoint}/someBad!identifier/_acl"
  next_uri = "/organizations/testorg/#{endpoint}/someBad!identifier/_aclbad"
  if endpoint == "clients"
    # Except for clients - special logic to send them to erchef
    # as described  above
    erchef_uris << next_uri
  else
    acct_uris << next_uri
  end
 end

[:get, :post, :delete, :put].each do |method|
  describe "internal acct load balancer" do
    describe "routes" do
      erchef_uris.each do |location|
        it "a #{method} to #{location} --> erchef" do
          make_request :type => :intacct, :location => location,
                       :method => method
          body.should == "erchef: #{strip_location(location)}"
        end
      end
      acct_uris.each do |location|
        it "a #{method} to #{location} --> acct" do
          make_request :type => :intacct, :location => location,
                       :method => method
          body.should == "account: #{strip_location(location)}"
        end
      end
    end

    # TODO should this really be different for internal?
    it "accepts a missing X-Ops-UserId by replying to a #{method} with 200 when it's missing" do
      make_request :type => :intchef, :no_user => true,
                   :method => method
      code.should == "200"
    end
  end

  # For internal, anything goes - we don't look at version.
  describe "internal account lb ignores version header on a #{method}" do
    it "and responds to blank version header with 200" do
      make_request :type => :intacct, :version => "",
                   :method => method
      code.should == "200"
    end
    it "and responds to valid header with 200" do
      make_request :type => :intacct, :no_version => false,
                   :method => method
      code.should == "200"
    end
    it "and responds to missing version header with 200" do
      make_request :type => :intacct, :no_version => true,
                   :method => method
      code.should == "200"
    end
  end
end
