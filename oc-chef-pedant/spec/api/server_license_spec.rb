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
      it "can get all users", :smoke do
        get(request_url, platform.superuser).should look_like({
            :status => 200,
            :body => users_body
          })
      end
    end

    # context "failure case", automate: true do
    context "when not having valid license",  if: ENV["IS_AUTOMATE"] == "true" do
      before(:all) do
        puts "applying expired license"
        puts ENV['A2_EXPIRED_LICENSE']
        system("chef-automate license apply -f \"eyJhbGciOiJFUzUxMiIsInR5cCI6IkpXVCJ9.eyJpZCI6ImEwYmMzZmMxLWFiOGUtNDI4ZC1hODZiLTU5MTE4ZDM3ZjYzOSIsInZlcnNpb24iOiIxIiwidHlwZSI6InRyaWFsIiwiZ2VuZXJhdG9yIjoiY2hlZi9saWNlbnNlLTIuMC4wIiwia2V5X3NoYTI1NiI6ImUwZGYyOGM4YmM2ODE1MGVkYmZlZjk4Y2Q2YjdkYzQzOWMxZjgwYzdlN2VmNzQ3ODkzYTY4OTNhMmY3YjYwZjciLCJnZW5lcmF0aW9uX2RhdGUiOnsic2Vjb25kcyI6MTcxOTY2NzUxM30sImN1c3RvbWVyIjoiU3RhbmRhcmQgQmFuayBSZW5ld2FsIFN0YXJ0IERhdGUgMjAyNC0wNy0wMSIsImN1c3RvbWVyX2lkIjoiNTRBOTNGOUEtOEE2RC00RERGLUJCQTktRkE4MDE0MkQxNTI1IiwiY3VzdG9tZXJfaWRfdmVyc2lvbiI6IjEiLCJlbnRpdGxlbWVudHMiOlt7Im5hbWUiOiJDaGVmIEF1dG9tYXRlIiwibWVhc3VyZSI6Im5vZGVzIiwibGltaXQiOi0xLCJzdGFydCI6eyJzZWNvbmRzIjoxNzE5NjE5MjAwfSwiZW5kIjp7InNlY29uZHMiOjE3MjIyOTc1OTl9fV19.AErwHcnIs7TQoWPEj35Fj-_zPOPIHAXH1YBpm7pUItchNhAMJhdlajhIlS0byVhmS479jf9mn9uCGTos9vsy4w9eAZV2uUUlXB9uOzd6QWEv_FpX2_CsfrpNQfkRiaZ21IgKySXEfmoNbbLWBXPC9lOxhwg2HkbH4aB0lkBUVqTHwE9D\"")
        system("sleep 100")
        puts "expired license applied"
        puts system("chef-automate license status")
      end
      after(:all) do
        system("chef-automate license apply \"$A2_LICENSE\"")
        system("sleep 100")
        puts "valid license applied"
        puts system("chef-automate license status")
      end

      it "returns 403", :smoke  do
        puts get(request_url, platform.superuser)
        get(request_url, platform.superuser).should look_like({
            :status => 403
          })
      end
      # it "can get status", automate: true do
      #   get(status_url, platform.superuser).should look_like({
      #       :status => 200
      #     })
      # end
    end
  end # context /users/<name> endpoint
end # describe users