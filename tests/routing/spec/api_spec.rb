require 'spec_helper.rb'
include SpecHelper

not_found_uris = [
  "/organizations/testorg/principals", # Internal-only
  "/organizations/testorg/principals/",
  "/organizations/testorg/principals/asd",
  "/organizations/TestOrg", # invalid identifier for org, which must be all lower.
  "/organization",
  "/organizations/TestOrg/",
  "/organizations/TestOrg/abc",
  "/bad/path1", # just plain doesn't exist
  "/",          # nope, this isn't a valid APi path either
  "/authenticate_user/",
  "/authenticate_user/bad",
  "/verify_password/",
  "/verify_password/bad",
  "/users/borg//association_requests0abc",
  "/users/borg/association_requests0abc",
  # int only
  "/internal-organization",
  "/internal-organizations"
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
}

%w{cookbooks data roles sandboxes environments clients nodes}.each do |endpoint|
  erchef_uris << "/organizations/testorg/#{endpoint}"
  erchef_uris << "/organizations/testorg/#{endpoint}/"
  erchef_uris << "/organizations/testorg/#{endpoint}/avalue"
  erchef_uris << "/organizations/testorg/#{endpoint}/avalue/"
  erchef_uris << "/organizations/testorg/#{endpoint}/avalue/asubvalue"
  # note that rules do not check secondary identifiers, and will allow this through
  erchef_uris << "/organizations/testorg/#{endpoint}/B@dIdentifier"
end

acct_uris = [
  "/authenticate_user",
  "/verify_password",
  "/users",
  "/users/good",
  "/users/bOrg", # bad identiifer, but we don't currently check that in the LB.
  "/users/borg/association_requests",
  "/users/borg/association_requests/",
  "/users/borg/association_requests/abc",
  "/users/borg//association_requests",
  "/users/borg//association_requests",
  "/users/borg//association_requests/abc",
  "/users/borg///association_requests/abc", # note that while the rules themselves don't permit '///', it appears
                                            # that nginx strips out the duplicates and passes it to the route
                                            # script as if it's a normal URI
]

%w{groups users containers association_requests}.each do |endpoint|
  acct_uris << "/organizations/testorg/#{endpoint}"
  acct_uris << "/organizations/testorg/#{endpoint}/"
  acct_uris << "/organizations/testorg/#{endpoint}/valid_identifier"
  acct_uris << "/organizations/testorg/#{endpoint}/valid_identifier/subidentifier"
  acct_uris << "/organizations/testorg/#{endpoint}/BadI!dentifier"
end
acct_uris << "/organizations/testorg/users/someuser/assocation_requests"
acct_uris << "/organizations/testorg/users/someuser/assocation_requests09"


# _acl and _acl/.*  always goes to account no matter what comes before it.
# Migration Stage 3: this will be changed to erchef after an interim
# period of acct-erchef
%w{cookbooks data roles sandboxes environments
   groups containers clients nodes reports}.each do |endpoint|
  acct_uris << "/organizations/testorg/#{endpoint}/someidentifier/_acl"
  acct_uris << "/organizations/testorg/#{endpoint}/_acl"
  acct_uris << "/organizations/testorg/#{endpoint}/someBad!identifier/_acl"
  acct_uris << "/organizations/testorg/#{endpoint}/someidentifier/_acl/grant"
end

# But not if it isn't properly formed.
erchef_uris << "/organizations/testorg/nodes/mynode/_aclextra"

report_uris = [
  "/organizations/myorg/reports/nodes/mynode/runs",
  "/organizations/myorg/reports/nodes",
  "/organizations/myorg/reports/nodes/mynode/runs/123",
  "/organizations/myorg/reports/something_else",
  "/organizations/myorg/reports/",
  "/organizations/myorg/reports"
]


# Internal and external API LBs should behave identically for these tests
[:get, :post, :delete, :put].each do |method|
  [:internal, :external].each do |lbtype|
    describe "api-#{lbtype} load balancer" do
      describe "routes" do
        erchef_uris.each do |location|
          it "a #{method} to #{location} --> erchef" do
            make_request :type => lbtype, :location => location
            body.should == "erchef: #{strip_location(location)}"
          end
        end

        acct_uris.each  do |location|
          it "a #{method} to #{location} -->  acct" do
            make_request :type => lbtype, :location => location,
                         :method => method
            body.should == "account: #{strip_location(location)}"
          end
        end

        report_uris.each do |location|
          it "a #{method} to #{location} --> reports" do
            # The default reporting version header is only required for a
            # small subset of reporting URLs, but the intent here is to ensure
            # that it routes correctly when all prerequisite conditions are met.
            # See route_checks_spec.rb for tests ensuring proper behavior
            # when this header is not present.
            make_request :type => lbtype, :location => location,
                         :method => method,
                         :headers => { REPORTING_PROTO_HEADER => "anything"}
            body.should == "reporting: #{strip_location(location)}"
          end
        end

        not_found_uris.each do |location|
          it "a #{method} to #{location} as 404" do
            make_request :type => lbtype, :location => location,
                         :method => method
            code.should == "404"
          end
        end
      end

      describe "properly handles non-secure #{method} requests by" do
        it "redirecting to https with a 301" do
          dest = "/organizations/test-org/nodes"
          make_request :type => "#{lbtype}_http".to_sym, :location => dest,
                       :method => method
          code.should == "301"
        end

        it "retaining simple query string in the redirect location" do
          dest = "/organizations/test-org/nodes?testval=1"
          make_request :type => "#{lbtype}_http".to_sym, :location => dest,
                       :method => method
          # match on the end of string here b/c the actual server name returned
          # will be populated from the hostnames data bag
          response["location"].end_with?(dest).should == true
        end

        it "retaining complex query string in the redirect location" do
          dest = "/organizations/test-org/nodes?testval=1&name=my%40name+test"
          make_request :type => "#{lbtype}_http".to_sym, :location => dest,
                       :method => method
          response["location"].end_with?(dest).should == true
        end
      end

      it "rejects a missing X-Ops-UserId with code.with 401" do
        make_request :type => lbtype, :no_user => true, method => :method
        code.should == "401"
      end
    end
  end
end

# Version compliance is enforced for external rules only.
describe "external api lb handles chef version by correctly by responding to" do
  VALID_CHEF_VERSIONS.each do |version|
    it "#{version} with 200" do
      make_request :type => :external, :version => version
      code.should == "200"
    end
  end
  INVALID_CHEF_VERSIONS.each do |version|
    it "#{version} with 400" do
      make_request :type => :external, :version => version
      code.should == "400"
    end
  end
  it "missing version header with an html document and a 200" do
    make_request :type => :external, :no_version => true
    code.should == "200"
    response["content-type"].should == "text/html"
  end
  it "missing version header and a bad url with an html document and a 200" do
    make_request :type => :external, :no_version => true, :location => "/any/location"
    code.should == "200"
    response["content-type"].should == "text/html"
  end
end

# For internal, anything goes - we don't look at version.
describe "internal api lb handles chef version by correctly responding to" do
  VALID_CHEF_VERSIONS.concat(INVALID_CHEF_VERSIONS).each do |version|
    it "#{version} with 200 " do
      make_request :type => :internal, :version => version
      code.should == "200"
    end
  end
  it "missing version header with 200" do
    make_request :type => :internal, :no_version => true
    code.should == "200"
  end
end

# TODO  - behavior of _route endpoint, with whitelist ok  (results ext, 404 int
# TODO - behavior of _route endpoint, with no whitelist (404)
#  NOTE: there is curently no darklaunch switching of upstreams.  If we add
#        any, we'll also need to add associated tests
