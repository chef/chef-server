# Contains tests around darklaunch rules
require 'spec_helper.rb'
include SpecHelper

erchef_acct_darklaunch_endpoints = %w{ containers groups }

describe "internal account" do
  erchef_acct_darklaunch_endpoints.each do |endpoint|
    [:get, :post, :delete, :put].each do |method|
      describe "when couchdb_#{endpoint} set to false" do
        before :all do
          @redis = redis_inst(:intacct)
          darklaunch_org_for_api(@redis, DEFAULT_ORG_NAME, endpoint)
        end
        it "routes a #{method} to erchef" do
          location = "/organizations/#{DEFAULT_ORG_NAME}/#{endpoint}/someobject"
          make_request :type => :intacct, :location => location
          body.should == "erchef: #{strip_location(location)}"
        end
        it "routes _acl for #{method} to account" do
          location = "/organizations/#{DEFAULT_ORG_NAME}/#{endpoint}/someobject/_acl"
          make_request :type => :intacct, :location => location
          body.should == "account: #{strip_location(location)}"
        end
      end
      describe "when couchdb_#{endpoint} set to true" do
        before :all do
          @redis = redis_inst(:intacct)
          undarklaunch_org_for_api(@redis, DEFAULT_ORG_NAME, endpoint)
        end
        it "routes a #{method} to account" do
          location = "/organizations/#{DEFAULT_ORG_NAME}/#{endpoint}/someobject"
          make_request :type => :intacct, :location => location
          body.should == "account: #{strip_location(location)}"
        end
        it "routes _acl for a #{method} to account" do
          location = "/organizations/#{DEFAULT_ORG_NAME}/#{endpoint}/someobject/_acl"
          make_request :type => :intacct, :location => location
          body.should == "account: #{strip_location(location)}"
        end
      end
    end
  end
end
describe "api" do
  # API darklaunch tests
  erchef_acct_darklaunch_endpoints.each do |endpoint|
    [:get, :post, :delete, :put].each do |method|
      [:internal, :external].each do |lbtype|
        describe "#{lbtype} when couchdb_#{endpoint} set to false" do
          before :all do
            @redis = redis_inst(lbtype)
            darklaunch_org_for_api(@redis, DEFAULT_ORG_NAME, endpoint)
          end
          it "routes a #{method} to erchef" do
            location = "/organizations/#{DEFAULT_ORG_NAME}/#{endpoint}/someobject"
            make_request :type => lbtype, :location => location
            body.should == "erchef: #{strip_location(location)}"
          end
          it "routes _acl for #{method} to account" do
            location = "/organizations/#{DEFAULT_ORG_NAME}/#{endpoint}/someobject/_acl"
            make_request :type => lbtype, :location => location
            body.should == "account: #{strip_location(location)}"
          end
        end
        describe "#{lbtype} acl request when couchdb_#{endpoint} set to false" do
          before :all do
            @redis = redis_inst(lbtype)
            darklaunch_org_for_api(@redis, DEFAULT_ORG_NAME, endpoint)
          end
          it "routes a #{method} to account" do
            location = "/organizations/#{DEFAULT_ORG_NAME}/#{endpoint}/someobject/_acl"
            make_request :type => lbtype, :location => location
            body.should == "account: #{strip_location(location)}"
          end
        end

        # This one is a bit redundant since technically these org routes in non-darklaunched
        # mode are captured in the other api routing tests, but I capture them here to be explicit
        # of expected behavior for something that *could* be darklaunched, but darklaunch is not enabled.
        describe "#{lbtype} when couchdb_#{endpoint} set to true" do
          before :all do
            @redis = redis_inst(lbtype)
            undarklaunch_org_for_api(@redis, DEFAULT_ORG_NAME, endpoint)
          end
          it "routes a #{method} to account" do
            location = "/organizations/#{DEFAULT_ORG_NAME}/#{endpoint}/someobject"
            make_request :type => lbtype, :location => location
            body.should == "account: #{strip_location(location)}"
          end
        end
      end
    end
  end
end
