
context "Server API Versioning", :server_api_version, :smoke do
  it "GET /server_api_version should respond with valid current server api version data" do
    r = get("#{platform.server}/server_api_version", superuser)
    r.should look_like(status_for_json: 200)
    data = JSON.parse(r)
    data.has_key?("min_api_version").should eql true
    data.has_key?("max_api_version").should eql true
    data["min_api_version"].should be_a(Fixnum)
    data["max_api_version"].should be_a(Fixnum)
    expect(data["min_api_version"]).to be <= data["max_api_version"]
  end

  context "version validation should occur before most other validation" do
    it "invalid method should fail for version, not method" do
      # These are invalid operations for the endpoint in use, and would normally result in a 405
      put("#{platform.server}/license", superuser, :payload => {}, :api_version => "invalid").should  have_status_code 406
      post("#{platform.server}/license", superuser, :payload => {}, :api_version => "invalid").should have_status_code 406
      delete("#{platform.server}/license", superuser, :api_version => "invalid").should have_status_code 406
    end
    it "invalid content should fail for version, not content" do
      # This would ordinarily be a 400
      post("#{platform.server}/users", superuser, :payload => {}, :api_version => "invalid").should have_status_code 406
    end
  end

  context "any request should validate requested server api version" do
    before(:all) do
      r = get("#{platform.server}/server_api_version", superuser)
      data = JSON.parse(r)
      @min_version = data["min_api_version"]
      @max_version = data["max_api_version"]
    end

    it "and it should reply with the error message as specified in the RFC" do
      expected_body = { "error" => "invalid-x-ops-server-api-version", "message" => "Specified version invalid not supported",
                        "min_version" => @min_version, "max_version" => @max_version}
      get("#{platform.server}/license", superuser, :api_version => "invalid").should look_like(status: 406,
                                                                                               body_exact: expected_body)
    end
    it "and rejects when version is not a number" do
      get("#{platform.server}/license", superuser, :api_version => "invalid").should have_status_code 406

    end
    it "and rejects when version is higher than what's supported" do
      get("#{platform.server}/license", superuser, :api_version => @max_version + 1).should have_status_code 406

    end
    it "and rejects when version is lower than what's supported" do
      get("#{platform.server}/license", superuser, :api_version => @min_version - 1).should have_status_code 406
    end
    it "and accepts when version is exactly the minimum of what's supported" do

      get("#{platform.server}/license", superuser, :api_version => @min_version).should have_status_code 200
    end
    it "and accepts when version is exactly the maximum of what's supported" do
      get("#{platform.server}/license", superuser, :api_version => @min_version).should have_status_code 200
    end

    it "and accepts when it's in valid range of what's supported" do
      # Note that until we rev api versiona gain, this is the same as exactly maximum...
      get("#{platform.server}/license", superuser, :api_version => @min_version + 1).should have_status_code 200
    end

    it "and accept it when it's not specified" do
      get("#{platform.server}/license", superuser, :api_version => nil).should have_status_code 200
    end
  end
end
