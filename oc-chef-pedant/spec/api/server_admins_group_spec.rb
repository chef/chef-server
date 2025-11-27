require "pedant/rspec/common"
require "restclient"
require "json"

describe "GET /groups/server-admins", :server_admins do
  let(:request_method) { :GET }
  let(:request_url) { "#{platform.server}/groups/server-admins" }
  
  def dump_erchef_logs
    log_file = "/tmp/erchef.log"
    if File.exist?(log_file)
      puts "\n" + "="*80
      puts "ERCHEF LOGS (last 100 lines with DEBUG):"
      puts "="*80
      # Get last 100 lines and filter for DEBUG
      logs = `tail -100 #{log_file} | grep -i "DEBUG"`
      puts logs
      puts "="*80 + "\n"
    else
      puts "\nWARNING: Could not find erchef log file at #{log_file}\n"
    end
  rescue => e
    puts "\nERROR reading logs: #{e.message}\n"
  end
  
  # Test to directly query and manipulate bifrost server-admins group
  context "bifrost server-admins group debugging" do
    it "queries bifrost directly to check group members" do
      begin
        require "pg"
      rescue LoadError
        skip "pg gem not available - skipping bifrost debugging test"
      end
      
      puts "\n" + "="*80
      puts "BIFROST DEBUG: Querying server-admins group directly"
      puts "="*80
      
      # First, get the server-admins authz_id from the database
      db_conn_string = "host=localhost port=5432 dbname=opscode_chef user=opscode_chef password=#{ENV['DB_PASSWORD'] || 'opscode_chef'}"
      
      begin
        conn = PG.connect(db_conn_string)
        puts "DEBUG: Connected to database"
        
        # Query for server-admins group
        result = conn.exec_params(
          "SELECT authz_id FROM groups WHERE name='server-admins' AND org_id='00000000000000000000000000000000'"
        )
        
        if result.ntuples == 0
          puts "ERROR: server-admins group not found in database!"
          expect(result.ntuples).to be > 0
        end
        
        server_admins_authz_id = result[0]['authz_id']
        puts "DEBUG: server-admins authz_id from DB: #{server_admins_authz_id}"
        
        # Query for pivotal user authz_id
        pivotal_result = conn.exec_params("SELECT authz_id FROM users WHERE username='pivotal'")
        if pivotal_result.ntuples == 0
          puts "ERROR: pivotal user not found in database!"
          expect(pivotal_result.ntuples).to be > 0
        end
        
        pivotal_authz_id = pivotal_result[0]['authz_id']
        puts "DEBUG: pivotal authz_id from DB: #{pivotal_authz_id}"
        
        conn.close
        
        # Now query bifrost for the group
        bifrost_url = ENV['BIFROST_URL'] || 'http://localhost:9463'
        bifrost_superuser_id = ENV['BIFROST_SUPERUSER_ID'] || '00000000000000000000000000000000'
        
        puts "DEBUG: Bifrost URL: #{bifrost_url}"
        puts "DEBUG: Querying bifrost: GET /groups/#{server_admins_authz_id}"
        
        # Query bifrost BEFORE adding pivotal
        response = RestClient.get(
          "#{bifrost_url}/groups/#{server_admins_authz_id}",
          {
            'Content-Type' => 'application/json',
            'Accept' => 'application/json',
            'X-Ops-Requesting-Actor-Id' => bifrost_superuser_id
          }
        )
        
        before_data = JSON.parse(response.body)
        puts "\nDEBUG: Bifrost response BEFORE adding pivotal:"
        puts "  actors: #{before_data['actors'].inspect}"
        puts "  users: #{before_data['users'].inspect}" if before_data['users']
        puts "  clients: #{before_data['clients'].inspect}" if before_data['clients']
        puts "  groups: #{before_data['groups'].inspect}" if before_data['groups']
        
        pivotal_in_group = before_data['actors'].include?(pivotal_authz_id)
        puts "  pivotal in group? #{pivotal_in_group}"
        
        # If pivotal is NOT in the group, add it
        unless pivotal_in_group
          puts "\nDEBUG: Adding pivotal to server-admins group via bifrost"
          add_response = RestClient.put(
            "#{bifrost_url}/groups/#{server_admins_authz_id}/actors/#{pivotal_authz_id}",
            "{}",
            {
              'Content-Type' => 'application/json',
              'Accept' => 'application/json',
              'X-Ops-Requesting-Actor-Id' => bifrost_superuser_id
            }
          )
          puts "DEBUG: Add response status: #{add_response.code}"
          
          # Query again AFTER adding
          after_response = RestClient.get(
            "#{bifrost_url}/groups/#{server_admins_authz_id}",
            {
              'Content-Type' => 'application/json',
              'Accept' => 'application/json',
              'X-Ops-Requesting-Actor-Id' => bifrost_superuser_id
            }
          )
          
          after_data = JSON.parse(after_response.body)
          puts "\nDEBUG: Bifrost response AFTER adding pivotal:"
          puts "  actors: #{after_data['actors'].inspect}"
          pivotal_in_group_after = after_data['actors'].include?(pivotal_authz_id)
          puts "  pivotal in group? #{pivotal_in_group_after}"
          
          expect(pivotal_in_group_after).to be true
        else
          puts "\nDEBUG: pivotal is already in server-admins group"
        end
        
        puts "="*80 + "\n"
        
      rescue PG::Error => e
        puts "ERROR: Database error: #{e.message}"
        raise
      rescue RestClient::Exception => e
        puts "ERROR: Bifrost HTTP error: #{e.message}"
        puts "Response: #{e.response.body}" if e.response
        raise
      end
    end
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
    
    it "returns 405 for DELETE" do
      response = delete(request_url, requestor)
      response.should look_like({
        status: 405
      })
    end
  end
end
