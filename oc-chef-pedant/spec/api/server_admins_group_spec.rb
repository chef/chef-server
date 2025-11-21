require "pedant/rspec/common"
require "restclient"
require "json"
require "securerandom"

describe "GET /groups/server-admins", :server_admins do
  let(:request_method) { :GET }
  let(:request_url) { "#{platform.server}/groups/server-admins" }
  
  def dump_erchef_logs
    # Erlang debug logs now go to stdout with sentinel '*** ERCHEF DEBUG LOG'
    # This method is kept for compatibility but is no longer needed
    puts "\n(Erlang debug logs should appear inline with test output - look for '*** ERCHEF DEBUG LOG')\n"
  end
  
  context "as pivotal/superuser" do
    let(:requestor) { platform.superuser }
    
    it "returns 200 OK with group membership" do
      puts "\n" + "="*80
      puts "TEST: returns 200 OK with group membership"
      puts "="*80
      
      response = get(request_url, requestor)
      
      dump_erchef_logs
      
      puts "\nResponse Status: #{response.code}"
      puts "Response Body: #{response.body}"
      parsed = JSON.parse(response)
      puts "Users field: #{parsed["users"].inspect}"
      puts "="*80 + "\n"
      
      response.should look_like({
        status: 200
      })
    end
    
    it "returns JSON with expected group structure" do
      # Capture logs BEFORE making the request
      puts "\n" + "="*80
      puts "MAKING REQUEST TO: #{request_url}"
      puts "="*80
      
      response = get(request_url, requestor)
      
      # Capture logs AFTER request
      dump_erchef_logs
      
      response.should look_like({
        status: 200
      })
      
      puts "\n" + "="*80
      puts "DEBUG: GET /groups/server-admins Response"
      puts "="*80
      puts "Status: #{response.code}"
      puts "Headers: #{response.headers.inspect}"
      puts "\nRaw Body:"
      puts response.body
      puts "\n" + "-"*80
      
      parsed = JSON.parse(response)
      
      puts "Parsed JSON:"
      require 'pp'
      pp parsed
      puts "\nSpecific fields:"
      puts "  actors: #{parsed["actors"].inspect}"
      puts "  users: #{parsed["users"].inspect}"
      puts "  clients: #{parsed["clients"].inspect}"
      puts "  groups: #{parsed["groups"].inspect}"
      puts "  name: #{parsed["name"].inspect}"
      puts "  groupname: #{parsed["groupname"].inspect}"
      puts "  orgname: #{parsed["orgname"].inspect}"
      puts "="*80 + "\n"
      
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
      # System returns "missing read permission" - the standard authorization error message
      error_msg = parsed["error"].is_a?(Array) ? parsed["error"].first : parsed["error"]
      error_msg.should match(/missing.*permission/i)
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
  
  # Note: We intentionally do NOT test clients here because clients are
  # organization-scoped and cannot authenticate to global endpoints by design.
  # Clients would receive 401 Unauthorized (not found in global context) rather
  # than 403 Forbidden, which is testing Chef Server's fundamental authentication
  # behavior rather than this endpoint's authorization logic.
  
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
  end
end
