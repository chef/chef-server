require 'pedant/rspec/common'

describe "opscode-account endpoint", :clients do
  let(:requestor){ platform.admin_user}

  context "Client ACLs", :acls do
    context "the initial validator client" do
      let(:client){ platform.test_org.validator.name }

      it "has the appropriate ACLs" do
        get(api_url("/clients/#{client}/_acl"), platform.admin_user).should look_like({
            :status => 200,
            :body_exact =>{
              "create" => {
                "actors" => ["pivotal"],
                "groups" => ["admins"]},
              "read" => {
                "actors" => ["pivotal"],
                "groups" => ["users","admins"]},
              "update" => {
                "actors" => ["pivotal"],
                "groups" => ["admins"]},
              "delete" => {
                "actors" => ["pivotal"],
                "groups" => ["users","admins"]},
              "grant" => {
                "actors" => ["pivotal"],
                "groups" => ["admins"]}
            }
          })
      end
    end

    context "a new validator client" do
      let(:client){ "pedant_test_validator" }
      before :each do
        post(api_url("/clients"), requestor,
          :payload => {"name" => client, "validator" => true}).should have_status_code(201)
      end

      after :each do
        delete(api_url("/clients/#{client}"), requestor)
      end

      it "has the appropriate ACLs" do
        actors = ["pivotal", requestor.name].uniq

        get(api_url("/clients/#{client}/_acl"), platform.admin_user).should look_like({
            :status => 200,
            :body =>{
              "create" => {
                "actors" => actors,
                "groups" => ["admins"]},
              "read" => {
                "actors" => actors,
                "groups" => ["users","admins"]},
              "update" => {
                "actors" => actors,
                "groups" => ["admins"]},
              "delete" => {
                "actors" => actors,
                "groups" => ["users","admins"]},
              "grant" => {
                "actors" => actors,
                "groups" => ["admins"]}
            }
          })
      end
    end
  end

  context "Admin User Group",:groups do
    let(:default_pedant_admin_names) { default_pedant_admins.map(&:name)}
    let(:default_pedant_admins) { [ platform.superuser ] + default_pedant_org_admins }

    # Selects pedant users that are marked as associated and admins
    let(:default_pedant_org_admins) { platform.users.select(&:associate).select(&:admin) }

    it "has the appropriate members" do
      get(api_url("/groups/admins"), platform.superuser).should look_like({
          :status => 200,
          :body_exact => {
            "actors" => default_pedant_admin_names,
            "users" => default_pedant_admin_names,
            "clients" => [],
            "groups" => [],
            "orgname" => platform.test_org.name,
            "name" => "admins",
            "groupname" => "admins"
          }
        })
    end
  end


  context "Client Group", :groups do
    let(:group_name){ "clients" }

    context "group retrieval" do
      let(:request_url) { api_url("groups/#{group_name}") }

      it 'retrieves the group' do
        all_clients = [platform.test_org.validator.name,
          platform.non_admin_client.name,
          platform.admin_client.name]

        get(request_url, requestor).should look_like({
            :status => 200,
            :body_exact => {
              'name' => group_name,
              'groupname' => group_name,
              'orgname' => platform.test_org.name,
              'actors' => all_clients,
              'clients' => all_clients,
              'users' => [],
              'groups' => []
            }})
      end
    end # group retrieval

    context "group ACL retrieval" do
      let(:request_url){ api_url("groups/#{group_name}/_acl") }

      it 'retrieves the ACL' do
        get(request_url, requestor).should look_like({
            :status => 200,
            :body_exact => {
              "create" => {
                "actors" => ["pivotal"],
                "groups" => ["admins"]},
              "read" => {
                "actors" => ["pivotal"],
                "groups" => ["admins"]},
              "update" => {
                "actors" => ["pivotal"],
                "groups" => ["admins"]},
              "delete" => {
                "actors" => ["pivotal"],
                "groups" => ["admins"]},
              "grant" => {
                "actors" => ["pivotal"],
                "groups" => ["admins"]}
            }})
      end
    end
  end

  context "Client Container", :containers do
    let(:container_name){ "clients" }

    it "retrieves the Clients container" do
      get(api_url("/containers/clients"), requestor).should look_like({
          :status => 200,
          :body_exact => {
            "containername" => "clients",
            "containerpath" => "clients"
          }
        })
    end

    it "retrieves the Clients container's ACL" do
      create_and_read_actors = ["pivotal"]

      get(api_url("/containers/clients/_acl"), requestor).should look_like({
          :status => 200,
          :body_exact => {
            "create" => {
              "actors" => create_and_read_actors,
              "groups" => ["admins"]},
            "read" => {
              "actors" => create_and_read_actors,
              "groups" => ["admins", "users"]},
            "update" => {
              "actors" => ["pivotal"],
              "groups" => ["admins"]},
            "delete" => {
              "actors" => ["pivotal"],
              # really?  Any user can nuke a client?
              "groups" => ["admins", "users"]},
            "grant" => {
              "actors" => ["pivotal"],
              "groups" => ["admins"]}
          }})
    end
  end

  context "Client Creation" do
    let(:new_client_name){ "pedant_testing_client" }
    after :each do
      delete(api_url("/clients/#{new_client_name}"), platform.admin_user)
    end

    context "by a non-validator client" do
      let(:requestor) { platform.non_admin_client }

      it "cannot create a client", :authorization do
        post(api_url("/clients"), requestor,
          :payload => {"name" => new_client_name}).should have_status_code(403)
      end
    end

    context "by a validator" do
      let(:requestor){ platform.test_org.validator }

      it "creates a new non-validator client" do
        post(api_url("/clients"), requestor,
          :payload => {"name" => new_client_name}).should look_like({
            :status => 201,
            :body => {
              'uri' => api_url("/clients/#{new_client_name}")
            }
          })
      end

      it "cannot create a new validator client", :authorization do
        post(api_url("/clients"), requestor,
             :payload => {"name" => new_client_name, "validator" => true}).should have_status_code(403)
      end

      it "has the validator removed from the new client's ACL" do
        post(api_url("/clients"), requestor, :payload => {"name" => new_client_name}).should have_status_code(201)

        get(api_url("/clients/#{new_client_name}/_acl"), platform.admin_user).should look_like({
            :status => 200,
            :body_exact => {
              "create"=>
              {"actors"=>["pivotal", new_client_name],
                "groups"=>["admins"]},
              "read"=>
              {"actors"=>["pivotal", new_client_name],
                "groups"=>["admins", "users"]},
              "update"=>
              {"actors"=>["pivotal", new_client_name],
                "groups"=>["admins"]},
              "delete"=>
              {"actors"=>["pivotal", new_client_name],
                "groups"=>["admins", "users"]},
              "grant"=>
              {"actors"=>["pivotal", new_client_name],
                "groups"=>["admins"]}}
          })
      end

      it "puts the new client into the 'clients' group" do
        post(api_url("/clients"), requestor, :payload => {"name" => new_client_name}).should have_status_code(201)

        all_clients = [platform.test_org.validator.name,
          platform.admin_client.name,
          platform.non_admin_client.name,
          new_client_name]

        get(api_url("/groups/clients"), platform.admin_user).should look_like({:status => 200,
            :body_exact => {
              'name' => 'clients',
              'groupname' => 'clients',
              'orgname' => platform.test_org.name,
              'actors' => all_clients,
              'clients' => all_clients,
              'users' => [],
              'groups' => []
            }})
      end

      skip "a new validator should have read / create permissions on clients container"

    end
  end

end
