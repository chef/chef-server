require "pedant/rspec/common"

describe "GET /groups/server-admins", :server_admins do
  let(:request_method) { :GET }
  let(:request_url) { "#{platform.server}/groups/server-admins" }
  
  context "as pivotal/superuser" do
    let(:requestor) { platform.superuser }
    
    it "returns 200 OK with group membership" do
      response = get(request_url, requestor)
      response.should look_like({
        status: 200
      })
    end
    
    it "returns JSON with expected group structure" do
      response = get(request_url, requestor)
      response.should look_like({
        status: 200
      })
      
      parsed = JSON.parse(response)
      
      # Verify required fields are present
      parsed.should have_key("actors")
      parsed.should have_key("users")
      parsed.should have_key("clients")
      parsed.should have_key("groups")
      parsed.should have_key("name")
      parsed.should have_key("groupname")
      
      # Verify group name is correct
      parsed["name"].should eq("server-admins")
      parsed["groupname"].should eq("server-admins")
      
      # Verify it's an array (even if empty)
      parsed["actors"].should be_kind_of(Array)
      parsed["users"].should be_kind_of(Array)
      parsed["clients"].should be_kind_of(Array)
      parsed["groups"].should be_kind_of(Array)
      
      # Pivotal should be in the users list
      parsed["users"].should include("pivotal")
      
      # Actors should contain all users and clients
      parsed["actors"].length.should eq(parsed["users"].length + parsed["clients"].length)
    end
  end
  
  context "as admin user (non-superuser)" do
    let(:requestor) { platform.admin_user }
    
    it "returns 403 Forbidden" do
      response = get(request_url, requestor)
      response.should look_like({
        status: 403
      })
    end
    
    it "returns error message about superuser requirement" do
      response = get(request_url, requestor)
      response.should look_like({
        status: 403
      })
      
      parsed = JSON.parse(response)
      parsed.should have_key("error")
      parsed["error"].should match(/superuser/i)
    end
  end
  
  context "as normal user (non-admin, non-superuser)" do
    let(:requestor) { platform.non_admin_user }
    
    it "returns 403 Forbidden" do
      response = get(request_url, requestor)
      response.should look_like({
        status: 403
      })
    end
  end
  
  context "as client" do
    let(:requestor) { platform.admin_client }
    
    it "returns 403 Forbidden" do
      response = get(request_url, requestor)
      response.should look_like({
        status: 403
      })
    end
  end
  
  context "with invalid methods" do
    let(:requestor) { platform.superuser }
    
    it "returns 405 for POST" do
      response = post(request_url, requestor, payload: {})
      response.should look_like({
        status: 405
      })
    end
    
    it "returns 405 for PUT" do
      response = put(request_url, requestor, payload: {})
      response.should look_like({
        status: 405
      })
    end
    
    it "returns 405 for DELETE" do
      response = delete(request_url, requestor)
      response.should look_like({
        status: 405
      })
    end
  end
end
