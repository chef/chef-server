describe "Testing Status API" do
  context "Status API", :status, :smoke do
    it "GET /_status should respond with valid health data" do
      r = get("#{platform.server}/_status", superuser)
      r.should look_like(status_for_json: 200)
      data = JSON.parse(r)
      include_version_in_status = Pedant::Config.include_version_in_status
      data.key?("server_version").should eql include_version_in_status
      data.key?("status").should eql true
      data.key?("upstreams").should eql true
      data.key?("keygen").should eql true
      data.key?("indexing").should eql true
      data["status"].should eql "pong"
    end
  end
end
