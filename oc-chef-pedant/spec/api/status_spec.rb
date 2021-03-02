
describe "Testing Status API with config", :config do
  let (:config) { JSON.parse(IO.read("/etc/opscode/chef-server-running.json"))['private_chef'] }
  context "Status API", :status, :smoke do
    it "GET /_status should respond with valid health data" do
      r = get("#{platform.server}/_status", superuser)
      r.should look_like(status_for_json: 200)
      data = JSON.parse(r)
      data.has_key?("server_version").to_s.should eql config['opscode-erchef']['include_version_in_status'].to_s
      data.has_key?("status").should eql true
      data.has_key?("upstreams").should eql true
      data.has_key?("keygen").should eql true
      data.has_key?("indexing").should eql true
      data["status"].should eql 'pong'
    end
  end
end