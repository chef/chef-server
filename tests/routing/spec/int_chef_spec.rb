# Tests routing rules specific to chef-internal LB vhosts

require 'spec_helper.rb'
include SpecHelper

not_found_uris = [
  "/organizations/TestOrg", # invalid identifier for org, which must be all lower.
  "/organizations/TestOrg/",
  "/organizations/TestOrg/abc",
  "/bad/path1", # just plain doesn't exist
  "/",          # nope, this isn't a valid APi path either
  "/authenticate_user/",
  "/authenticate_user/bad",
  "/verify_password/",
  "/verify_password/bad",
  "/users/borg//association_requests0abc",
  "/users/borg/association_requests0abc"
]

erchef_uris = %w{
  /organizations/testorg/environments/envname/nodes
  /organizations/testorg/environments/envname/nodes/
  /organizations/testorg/environments/envname/node
  /organizations/testorg/environments/envname/nodesbad
  /organizations/testorg/search
  /organizations/testorg/search/
  /organizations/testorg/search/blah
  /organizations/testorg/search?x=1
  /organizations/testorg/principals
  /organizations/testorg/principals/
  /organizations/testorg/principals/asd
}

%w{cookbooks data roles sandboxes environments clients nodes}.each do |endpoint|
  erchef_uris << "/organizations/testorg/#{endpoint}"
  erchef_uris << "/organizations/testorg/#{endpoint}/"
  erchef_uris << "/organizations/testorg/#{endpoint}/avalue"
  erchef_uris << "/organizations/testorg/#{endpoint}/avalue/"
  erchef_uris << "/organizations/testorg/#{endpoint}/avalue/asubvalue"
  erchef_uris << "/organizations/testorg/#{endpoint}/B@dIdentifier"
end

report_uris = [
  "/organizations/myorg/reports/nodes/mynode/runs",
  "/organizations/myorg/reports/nodes",
  "/organizations/myorg/reports/nodes/mynode/runs/123",
  "/organizations/myorg/reports/something_else",
  "/organizations/myorg/reports/",
  "/organizations/myorg/reports"
]

describe "chef internal load balancer" do
  describe "routes" do
    [:get, :post, :delete, :put].each do |method|
      erchef_uris.each do |location|
        it "a #{method} to #{location} --> erchef" do
          make_request :type => :intchef, :location => location,
                       :method => method
          body.should == "erchef: #{strip_location(location)}"
        end
      end

      report_uris.each do |location|
        it "a #{method} to #{location} --> reports" do
          make_request :type => :intchef, :location => location,
                       :method => method,
                       :headers => { REPORTING_PROTO_HEADER => "anything"}
          body.should == "reporting: #{strip_location(location)}"
        end
      end

      not_found_uris.each do |location|
        it "a #{method} to #{location} as 404" do
          make_request :type => :intchef, :location => location,
                       :method => method
          code.should == "404"
        end
      end
    end
  end

  it "consumes valid _acl requests without routing to acct" do
    location = "/organizations/testorg/environments/_acl"
    make_request :type => :intchef,
                 :location => location
    body.should == "erchef: #{strip_location(location)}"
  end

  # No need to test all of them,  but it should reject anything we send to acct
  describe "fails to route acct endpoints" do
    it "with a 404 for /organizations" do
        make_request :type => :intchef, :location => "/organizations"
        code.should == "404"
    end
  end

  # TODO should this really be different for internal?
  it "accepts a missing X-Ops-UserId by replying with 200 when it's missing" do
    make_request :type => :intchef, :no_user => true
    code.should == "200"
  end
end

# For internal, anything goes - we don't look at version.
describe "internal chef lb ignores version header" do
  it "and responds to blank version header with 200" do
    make_request :type => :intchef, :version => ""
    code.should == "200"
  end
  it "and responds to valid header with 200" do
    make_request :type => :intchef, :no_version => false
    code.should == "200"
  end
  it "and responds to missing version header with 200" do
    make_request :type => :intchef, :no_version => true
    code.should == "200"
  end
end
