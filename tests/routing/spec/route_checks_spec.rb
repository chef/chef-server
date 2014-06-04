require 'spec_helper.rb'
include SpecHelper

[:internal, :external, :intacct].each do |lbtype|
  describe "#{lbtype} lb route checks" do
    before do
      @redis = redis_inst(lbtype)
    end

    describe "and attempting to create an org" do
      after(:each) do
        enable_new_org_creation(@redis)
      end

      it "replies with 200 when creation not disabled" do
        make_request :type => lbtype, :location => "/organizations",
                     :method => :post
        code.should == "200"
      end

      it "replies with 503 when creation disabled" do
        disable_new_org_creation(@redis)
        make_request :type => lbtype, :location => "/organizations",
                     :method => :post
        code.should == "503"
      end
    end
  end
end

[:internal, :external, :intchef, :intacct].each do |lbtype|
  describe "#{lbtype} lb route checks" do
    before do
      @redis = redis_inst(lbtype)
    end
    describe "when a route is darklaunched" do
      before do
        set_route_darklaunch(@redis, valid_route_for(lbtype))
      end

      after do
        undarklaunch_org_for_route(@redis, DEFAULT_ORG_NAME, valid_route_for(lbtype))
        clear_route_darklaunch(@redis, valid_route_for(lbtype))
      end

      it "responds with 404 if my org isn't darklaunched for that route" do
        make_request :type => lbtype
        code.should == "404"
      end
      it "responds with 200 if my org is darklaunched for that route" do
        darklaunch_org_for_route(@redis, DEFAULT_ORG_NAME, valid_route_for(lbtype))
        make_request :type => lbtype
        code.should == "200"
      end

    end
    describe "when an org is in maintenance mode" do
      before do
        set_org_maint_mode(@redis, DEFAULT_ORG_NAME)
      end

      after do
        clear_org_maint_mode(@redis, DEFAULT_ORG_NAME)
      end

      it "responds to org-related requests with 503" do
        make_request :type => lbtype
        code.should == "503"
      end

      it "responds to the same request for a different org with 200" do
        make_request :type => lbtype, :location => valid_location_for(lbtype, "otherorg")
        code.should == "200"
      end
    end

    describe "when an org is blocked" do
      before do
        set_org_blocked(@redis, DEFAULT_ORG_NAME)
      end

      after do
        clear_org_blocked(@redis, DEFAULT_ORG_NAME)
      end

      it "responds with org-relaetd requests with 403" do
        make_request :type => lbtype
        code.should == "403"
      end

      it "responds to the same request for a different org with 200" do
        make_request :type => lbtype, :location => valid_location_for(lbtype, "otherorg")
        code.should == "200"
      end
    end

    describe "when a route is in maintenance mode" do
      before do
        set_route_maint_mode(@redis, valid_route_for(lbtype))
      end

      after do
        clear_route_maint_mode(@redis, valid_route_for(lbtype))
      end

      it "responds to requests to this route with 503" do
        make_request :type => lbtype
        code.should == "503"
      end
      # Requests to verify that other routes are still available will be
      # executed below.
    end
  end
end

# While most maint mode behavior can be tested by determinign
# valid target routes and locations based on destination lb type,
# here we need to construct an alternate route for the same
# lb type that is also valid.
#
describe "route checks for maint mode" do
  after(:each) do
    ["erchef", "acct", "reports"].each do |route|
      clear_route_maint_mode(redis_inst(:external), route)
      clear_route_maint_mode(redis_inst(:internal), route)
    end
  end

  it "erchef in maint, acct available via external api" do
    @redis = redis_inst(:external)
    set_route_maint_mode(@redis, "erchef")
    make_request :type => :external, :location => "/organizations/testorg/users"
    code.should == "200"
  end

  it "erchef in maint, acct available via internal api" do
    @redis = redis_inst(:internal)
    set_route_maint_mode(@redis, "erchef")
    make_request :type => :internal, :location => "/organizations/testorg/users"
    code.should == "200"
  end

  it "reports in maint, erchef available via internal chef " do
    @redis = redis_inst(:internal)
    set_route_maint_mode(@redis, "reports")
    make_request :type => :intchef, :location => "/organizations/testorg/nodes"
    code.should == "200"
  end

  it "acct in maint, clients available via internal acct" do
    @redis = redis_inst(:internal)
    set_route_maint_mode(@redis, "acct")
    make_request :type => :intacct, :location => "/organizations/testorg/clients/test"
    code.should == "200"
  end
end


[:internal, :external].each do |lbtype|
  describe "#{lbtype}-api route checks" do
    before do
      @redis = redis_inst(lbtype)
    end
    describe "when my org is tarpitted" do
      after(:each) do
        detarpit_upstream_for_org(@redis, "reports", DEFAULT_ORG_NAME)
        detarpit_upstream_for_org(@redis, "erchef", DEFAULT_ORG_NAME)
        detarpit_upstream_for_org(@redis, "acct", DEFAULT_ORG_NAME)
      end

      it "to erchef tarpit1 it goes there" do
        tarpit_upstream_for_org(@redis, "erchef", DEFAULT_ORG_NAME, "1")
        location =  "/organizations/testorg/nodes"
        make_request :type => lbtype, :location => location
        body.should == "erchef_tarpit1: #{strip_location(location)}"
      end
      it "to erchef tarpit2 it goes there" do
        tarpit_upstream_for_org(@redis, "erchef", DEFAULT_ORG_NAME, "2")
        location =  "/organizations/testorg/nodes"
        make_request :type => lbtype, :location => location
        body.should == "erchef_tarpit2: #{strip_location(location)}"
      end
      it "to reports tarpit1 it goes there" do
        tarpit_upstream_for_org(@redis, "reports", DEFAULT_ORG_NAME, "1")
        location =  "/organizations/testorg/reports/nodes/nodea/runs"
        make_request :type => lbtype, :location => location
        body.should == "reporting_tarpit1: #{strip_location(location)}"
      end
      it "to acct tarpit1 it goes there" do
        tarpit_upstream_for_org(@redis, "acct", DEFAULT_ORG_NAME, "1")
        location =  "/organizations/testorg/users"
        make_request :type => lbtype, :location => location
        body.should == "account_tarpit1: #{strip_location(location)}"
      end
      it "and a bad tarpit is specified a 502 is returned" do
        tarpit_upstream_for_org(@redis, "acct", DEFAULT_ORG_NAME, "2")
        location =  "/organizations/testorg/users"
        make_request :type => lbtype, :location => location
        code.should == "502"
      end
      it "and another org is not tarpitted that org does not go to a tarpit" do
        tarpit_upstream_for_org(@redis, "acct", DEFAULT_ORG_NAME, "1")
        location =  "/organizations/otherorg/users"
        make_request :type => lbtype, :location => location
        body.should == "account: #{strip_location(location)}"
      end
    end
    describe "against reporting node runs endpoint" do
      before do
        @reports_loc = "/organizations/myorg/reports/nodes/mynode/runs"
      end

      it "replies with 404 on POST with no X-Ops-Reporting-Protocol-Version" do
        make_request :type => lbtype, :location => @reports_loc, :method => :post
        code.should == "404"
      end

      it "replies with 404 on POST with empty X-Ops-Reporting-Protocol-Version" do
        make_request :type => lbtype, :location => @reports_loc,
                     :method => :post,
                     :headers => {  REPORTING_PROTO_HEADER => ""}
        code.should == "404"
      end

      it "replies with 200 on POST with valid X-Ops-Reporting-Protocol-Version" do
        make_request :type => lbtype, :location => @reports_loc,
                     :method => :post,
                     :headers => { REPORTING_PROTO_HEADER => "anything"}
        code.should == "200"
      end

      it "replies with 200 on GET with no X-Ops-Reporting-Protocol-Version" do
        make_request :type => lbtype, :location => @reports_loc, :method => :get
        code.should == "200"
      end

      it "replies with 200 on GET with empty X-Ops-Reporting-Protocol-Version" do
        make_request :type => lbtype, :location => @reports_loc,
                     :method => :get,
                     :headers => { REPORTING_PROTO_HEADER => ""}
        code.should == "200"
      end

      it "replies with 200 on GET with valid X-Ops-Reporting-Protocol-Version" do
        make_request :type => lbtype, :location => @reports_loc,
                     :method => :get,
                     :headers => { REPORTING_PROTO_HEADER => "anything" }
        code.should == "200"
      end
    end
  end
end

