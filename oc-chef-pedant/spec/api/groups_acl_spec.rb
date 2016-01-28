require 'pedant/rspec/common'
require 'pedant/acl'

describe "Groups ACL", :acl do
  include Pedant::ACL

  describe "returns expected response when trying to remove the admin group from any grant ACE" do
    # For now testing with groups, but potentially also want to test with other objects
    %w(admins clients users).each do |test_group|
      context "for #{test_group}" do
        let(:admin_username) { platform.admin_user.name }
        let(:acl_url) { "#{platform.server}/groups/#{test_group}/_acl" }
        let(:read_acl_url) { api_url("groups/#{test_group}/_acl") }
        let(:write_acl_url) { api_url("groups/#{test_group}/_acl/grant") }

        let(:actors) { ["pivotal"] }
        let(:groups) { [] }

        # Both the actors and groups keys must be present when doing a PUT
        # or else the server rejects the request
        let(:request_body) {{
          "grant" => {
            "actors" => actors,
            "groups" => groups
          }
        }}

        # Since we're playing with permissions, let's make sure we restore them
        # If these tests pass, this shouldn't be needed, but if they fail we
        # might be in a bad state, and then the restore is needed
        before :each do
          original_state = get(read_acl_url, superuser)
          # If we can't get the original state, die, because then we can't restore it
          original_state.should look_like({:status => 200})
          # The response is returned as a string that is valid JSON (in this case at least)
          # Parsing it turns it into a usable hash
          state_hash = JSON.parse original_state
          # We're only changing the grant ACE, so pull that out to be able to use it
          # for the restore
          @original_grant_perms = state_hash['grant']
        end

        after :each do
          payload = {"grant" => @original_grant_perms}
          # If the original permissions don't restore, die, as we have an issue
          put(write_acl_url, superuser,
              :payload => payload).should look_like({:status => 200})
        end

        it "returns Forbidden trying to remove grant ace from #{test_group} group using non-superuser", :authorization do
          response = put(write_acl_url, platform.admin_user,
                         :payload => request_body)
          response.should look_like({:status => 403})
        end

        it "returns OK trying to remove grant ace from #{test_group} group user superuser" do
          response = put(write_acl_url, superuser,
                         :payload => request_body)
          response.should look_like({:status => 200})
        end


      end
    end

   describe "returns success when trying to remove the admin group from the read ACE (a non-grant ACE)" do
    # This test exists to ensure the non-grant case works, as there was a bug in the code
    # where the code checking for the grant case was causing the non-grant case to fail

    # For now testing with groups, but potentially also want to test with other objects
    %w(admins clients users).each do |test_group|
      context "for #{test_group}" do
        let(:admin_username) { platform.admin_user.name }
        let(:acl_url) { "#{platform.server}/groups/#{test_group}/_acl" }
        let(:read_acl_url) { api_url("groups/#{test_group}/_acl") }
        let(:write_acl_url) { api_url("groups/#{test_group}/_acl/read") }

        let(:actors) { ["pivotal"] }
        let(:groups) { [] }

        # Both the actors and groups keys must be present when doing a PUT
        # or else the server rejects the request
        let(:request_body) {{
          "read" => {
            "actors" => actors,
            "groups" => groups
          }
        }}

        # Since we're playing with permissions, let's make sure we restore them
        # If these tests pass, this shouldn't be needed, but if they fail we
        # might be in a bad state, and then the restore is needed
        before :each do
          original_state = get(read_acl_url, superuser)
          # If we can't get the original state, die, because then we can't restore it
          original_state.should look_like({:status => 200})
          # The response is returned as a string that is valid JSON (in this case at least)
          # Parsing it turns it into a usable hash
          state_hash = JSON.parse original_state
          # We're only changing the read ACE, so pull that out to be able to use it
          # for the restore
          @original_perms = state_hash['read']
        end

        after :each do
          payload = {"read" => @original_perms}
          # If the original permissions don't restore, die, as we have an issue
          put(write_acl_url, superuser,
              :payload => payload).should look_like({:status => 200})
        end

        it "returns success trying to remove read ace from #{test_group} group" do
          response = put(write_acl_url, platform.admin_user,
                         :payload => request_body)
          response.should look_like({:status => 200})
        end

      end
    end
  end
 end

end
