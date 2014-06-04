# _route is exposed only on the internal API LB
# it provides a way to validate the upstream server and
# the darklaunch values that any given URL will make use of.
# These tests are geared towards validating the required functionality
# as used by other components: making sure that darklaunch values are present

require 'spec_helper.rb'
require 'json'

include SpecHelper


VALID_ROUTES_LOCATION = "/_route/organizations/#{DEFAULT_ORG_NAME}"

describe "_routes endpoint" do

  [:intchef, :external].each do |lbtype|
    it "request to #{lbtype} LB returns 404" do
      make_request :type => lbtype, :location => VALID_ROUTES_LOCATION
      code.should == "404"
    end
  end

  describe "request to int-account" do
    it "response with 200 because int-account accepts everything" do
      make_request :type => :intacct, :location => VALID_ROUTES_LOCATION
      code.should == "200"
    end
    it "does not respond with a valid JSON document" do
      expect {JSON.parse(body)}.to raise_error
    end
  end

  describe "request to internal LB" do
    before :all do
      make_request :type => :internal, :location => VALID_ROUTES_LOCATION
      @json_body = JSON.parse(body)
      @redis = redis_inst(:internal)
    end

    it "responds with 200" do
      code.should == "200"
    end

    it "responds with a valid JSON document" do
      expect {JSON.parse(body)}.to_not raise_error
    end

    %w{upstream_target route uri org_name
       config object_name endpoint}.each do |expected_key|
      it "that contains a value for #{expected_key}" do
        expect(@json_body.has_key?(expected_key)).to eq(true)
      end
    end
    describe "the document's 'config' key" do
      it "resolves to a valid hash" do
        config = @json_body['config']
        expect(config.class).to eq(Hash)
      end
      it "contains a 'merged' key" do
        config = @json_body['config']
        expect(config.has_key?('merged')).to eq(true)
      end

      describe "config['merged']" do
        it "resolves to a valid hash" do
          merged = @json_body['config']['merged']
          expect(merged.class).to eq(Hash)
        end
        it "resolves to a valid hash even when no values are present" do
          clear_all_defaults(@redis)
          clear_all_for_org(@redis, DEFAULT_ORG_NAME)
          make_request :type => :internal, :location => VALID_ROUTES_LOCATION
          json_body = JSON.parse(body)
          merged = json_body['config']['merged']
          expect(merged.class).to eq(Hash)
        end
      end
    end
  end
end

