# -*- coding: utf-8 -*-
require 'pedant/rspec/common'

describe "server license testing", :license do

  # Pedant has configurable test users.
  # Selects Pedant users that are marked as associated
  let(:default_pedant_user_names) { platform.users.select(&:associate).map(&:name).sort }
  let(:default_users_body)        { default_pedant_user_names.map { |user| {"user" => {"username" => user} } } }

  # context "/users endpoint", automate: true do
  context "/users endpoint" do
    let(:request_url) { "#{platform.server}/users" }
    let(:status_url) { "#{platform.server}/_status" }

    let(:users_body) do
      {
        # There are other users, but these are ours, so they should always be
        # somewhere in the userspace soup.
        "pivotal" => "#{request_url}/pivotal",
        platform.bad_user.name => "#{request_url}/#{platform.bad_user.name}",
        platform.admin_user.name => "#{request_url}/#{platform.admin_user.name}",
        platform.non_admin_user.name => "#{request_url}/#{platform.non_admin_user.name}",
      }
    end

    context "when having valid license" do
      it "can get all users", automate: true do
        get(request_url, platform.superuser).should look_like({
            :status => 200,
            :body => users_body
          })
      end

      it "can get status", automate: true do
        get(status_url, platform.superuser).should look_like({
            :status => 200
          })
      end
    end

    # context "failure case", automate: true do
    context "when not having valid license" do
      before(:all) do
        system("chef-automate license apply \"$A2_EXPIRED_LICENSE\"")
        system("sleep 30")
      end
      after(:all) do
        system("chef-automate license apply \"$A2_LICENSE\"")
        system("sleep 30")
      end

      it "returns 403", automate: true do
        puts get(request_url, platform.superuser)
        get(request_url, platform.superuser).should look_like({
            :status => 403
          })
      end
      it "can get status", automate: true do
        get(status_url, platform.superuser).should look_like({
            :status => 200
          })
      end
    end
  end # context /users/<name> endpoint
end # describe users