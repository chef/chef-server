#
# -*- indent-level => 4;indent-tabs-mode: nil; fill-column: 92 -*-
# ex => ts=4 sw=4 et
#
# Author => : Prajakta Purohit (<prajakta@chef.io>)
# Copyright => : Copyright 2014-2018 Chef Software, Inc.
#

require 'pedant/rspec/common'
require 'pedant/rspec/node_util'


describe "Controls API Endpoint", :controls do

  if Pedant::Config.actions_enabled

    describe "v0.1.0" do
      include Pedant::RSpec::NodeUtil

      # The reason for this is that we need to verify
      # that the principal endpoint functions properly in OPC, as
      # currently the only consumer of this endpoint is chef-client-audit which
      # absolutely requires the OPC control endpoint to work.
      let(:input) {
        {
          :message_type => "control_groups",
          :message_version => "0.1.0",
          :organization_name => "ponyville",
          :chef_server_fqdn => "api.opscode.piab",
          :recorded_at => "2014-11-07T14:53:58Z",
          :remote_hostname => "33.33.33.1",
          :request_id => "g3IAA2QAEGVyY2hlZkAxMjcuMC4wLjEDAABM1wAAAAAAAAAA",
          :node_name => "client.opscode.piab",
          :id => "a4247480-bb92-42a5-8116-3729dcc486eb",
          :run_id => "76296628-7de0-4509-adaa-9a525409142d",
          :control_groups => [
            {
              :name => "mysql audit",
              :status => "failure",
              :number_success => 1,
              :number_failed => 1,
              :id => "59186fdc-2d0a-4653-af83-bbe79302fcaf",
              :controls => [
                {
                  :name => "should be installed",
                  :status => "success",
                  :details => "null",
                  :resource_type => "Package",
                  :resource_name => "mysql",
                  :context => [],
                  :sequence_number => 1
                },
                {
                  :name => "should have mysql service",
                  :status => "failure",
                  :details => "expected Service \"mysql\" to be enabled",
                  :resource_type => "null",
                  :resource_name => "null",
                  :context => [],
                  :sequence_number => 2
                }
              ]
            }
          ]
        }
      }
      let(:client){ platform.create_client('client.opscode.piab') }
      let(:clientwithnonode){ platform.create_client('clientwithnonode.opscode.piab') }
      let(:normaluser){ normal_user }
      let(:adminuser){ admin_user }

      context 'POST /controls' do
        after :each do
          delete(api_url("clients/client.opscode.piab"), platform.superuser)
          delete("#{platform.server}/clients/client.opscode.piab", platform.superuser)

          delete(api_url("clients/clientwithnonode.opscode.piab"), platform.superuser)
          delete("#{platform.server}/clients/clientwithnonode.opscode.piab", platform.superuser)
        end

        it 'returns a 201 ("Created") for client' do
          post(api_url("/controls"),
              client,
              :payload => input ) do |response|
                response.should look_like({
                  :status => 201
                })
              end
        end

        it 'returns a 403 ("Forbidden") for normal_user' do
          post(api_url("/controls"),
              normaluser,
              :payload => input ) do |response|
                response.should look_like({
                  :status => 403
                })
              end
        end

        it 'returns a 403 ("Forbidden") for admin_user' do
          post(api_url("/controls"),
              adminuser,
              :payload => input ) do |response|
                response.should look_like({
                  :status => 403
                })
              end
        end

      end

      context 'GET /controls' do
        after :each do
          delete(api_url("clients/client.opscode.piab"), platform.superuser)
          delete("#{platform.server}/clients/client.opscode.piab", platform.superuser)

          delete(api_url("clients/clientwithnonode.opscode.piab"), platform.superuser)
          delete("#{platform.server}/clients/clientwithnonode.opscode.piab", platform.superuser)

        end

        it 'returns a 405 ("Forbidden") for client' do
          get(api_url("/controls"),
              client,
              :payload => input ) do |response|
                response.should look_like({
                  :status => 405
                })
              end
        end

        it 'returns a 405 ("Method Not Allowed") for normal_user' do
          get(api_url("/controls"),
              normaluser,
              :payload => input ) do |response|
                response.should look_like({
                  :status => 405
                })
              end
        end

        it 'returns a 405 ("Method Not Allowed") for admin_user' do
          get(api_url("/controls"),
              adminuser,
              :payload => input ) do |response|
                response.should look_like({
                  :status => 405
                })
              end
        end

      end

      context "node does not exist" do
        it 'returns a 403 ("Forbidden") for client' do
          post(api_url("/controls"),
              clientwithnonode,
              :payload => input ) do |response|
                response.should look_like({
                  :status => 403
                })
              end
        end

      end

      context 'invalid input' do
        before :each do
          @input = input
        end
        after :each do
          @input = input
          delete(api_url("clients/client.opscode.piab"), platform.superuser)
          delete("#{platform.server}/clients/client.opscode.piab", platform.superuser)

          delete(api_url("clients/clientwithnonode.opscode.piab"), platform.superuser)
          delete("#{platform.server}/clients/clientwithnonode.opscode.piab", platform.superuser)
        end

        context 'missing run_id' do
          it 'returns a 400 ("Bad Request") for client missing run_id' do
            post(api_url("/controls"),
                client,
                :payload => @input.delete('run_id') ) do |response|
                  response.should look_like({
                    :status => 400
                  })
                end
          end
        end

        context 'missing node_name' do
          it 'returns a 400 ("Bad Request") for client missing node_name' do
            post(api_url("/controls"),
                client,
                :payload => @input.delete('node_name') ) do |response|
                  response.should look_like({
                    :status => 400
                  })
                end
          end
        end

        context 'missing control_groups' do
          it 'returns a 400 ("Bad Request") for client missing control_groups' do
            post(api_url("/controls"),
                client,
                :payload => @input.delete('control_groups') ) do |response|
                  response.should look_like({
                    :status => 400
                  })
                end
          end
        end

      end
    end # end 0.1.0 tests

    describe "v0.1.1" do
      include Pedant::RSpec::NodeUtil

      # The reason for this is that we need to verify
      # that the principal endpoint functions properly in OPC, as
      # currently the only consumer of this endpoint is chef-client-audit which
      # absolutely requires the OPC control endpoint to work.

      let(:input) {
        {
          :message_type => "control_groups",
          :message_version => "0.1.1",
          :organization_name => "ponyville",
          :chef_server_fqdn => "api.opscode.piab",
          :recorded_at => "2014-11-07T14:53:58Z",
          :remote_hostname => "33.33.33.1",
          :request_id => "g3IAA2QAEGVyY2hlZkAxMjcuMC4wLjEDAABM1wAAAAAAAAAA",
          :node_name => "client.opscode.piab",
          :id => "a4247480-bb92-42a5-8116-3729dcc486eb",
          :run_id => "76296628-7de0-4509-adaa-9a525409142d",
          :start_time => "2014-12-03T00:00:42Z",
          :end_time => "2014-12-03T00:00:48Z",
          :control_groups => [
            {
              :name => "mysql audit",
              :status => "failure",
              :number_success => 1,
              :number_failed => 1,
              :id => "59186fdc-2d0a-4653-af83-bbe79302fcaf",
              :controls => [
                {
                  :name => "should be installed",
                  :status => "success",
                  :details => "null",
                  :resource_type => "Package",
                  :resource_name => "mysql",
                  :context => [],
                  :sequence_number => 1
                },
                {
                  :name => "should have mysql service",
                  :status => "failure",
                  :details => "expected Service \"mysql\" to be enabled",
                  :resource_type => "null",
                  :resource_name => "null",
                  :context => [],
                  :sequence_number => 2
                }
              ],
              :cookbook_name => "tyler-mysql",
              :cookbook_version => "0.1.0",
              :recipe_name => "default",
              :line_number => 19
            }
          ]
        }
      }

      let(:client){ platform.create_client('client.opscode.piab') }
      let(:clientwithnonode){ platform.create_client('clientwithnonode.opscode.piab') }
      let(:normaluser){ normal_user }
      let(:adminuser){ admin_user }

      context 'POST /controls' do
        after :each do
          delete(api_url("clients/client.opscode.piab"), platform.superuser)
          delete("#{platform.server}/clients/client.opscode.piab", platform.superuser)

          delete(api_url("clients/clientwithnonode.opscode.piab"), platform.superuser)
          delete("#{platform.server}/clients/clientwithnonode.opscode.piab", platform.superuser)
        end

        it 'returns a 201 ("Created") for client' do
          post(api_url("/controls"),
              client,
              :payload => input ) do |response|
                response.should look_like({
                  :status => 201
                })
              end
        end

        it 'returns a 403 ("Forbidden") for normal_user' do
          post(api_url("/controls"),
              normaluser,
              :payload => input ) do |response|
                response.should look_like({
                  :status => 403
                })
              end
        end

        it 'returns a 403 ("Forbidden") for admin_user' do
          post(api_url("/controls"),
              adminuser,
              :payload => input ) do |response|
                response.should look_like({
                  :status => 403
                })
              end
        end

      end

      context 'GET /controls' do
        after :each do
          delete(api_url("clients/client.opscode.piab"), platform.superuser)
          delete("#{platform.server}/clients/client.opscode.piab", platform.superuser)

          delete(api_url("clients/clientwithnonode.opscode.piab"), platform.superuser)
          delete("#{platform.server}/clients/clientwithnonode.opscode.piab", platform.superuser)
        end

        it 'returns a 405 ("Forbidden") for client' do
          get(api_url("/controls"),
              client,
              :payload => input ) do |response|
                response.should look_like({
                  :status => 405
                })
              end
        end

        it 'returns a 405 ("Method Not Allowed") for normal_user' do
          get(api_url("/controls"),
              normaluser,
              :payload => input ) do |response|
                response.should look_like({
                  :status => 405
                })
              end
        end

        it 'returns a 405 ("Method Not Allowed") for admin_user' do
          get(api_url("/controls"),
              adminuser,
              :payload => input ) do |response|
                response.should look_like({
                  :status => 405
                })
              end
        end

      end

      context 'node does not exist' do
        it 'returns a 403 ("Forbidden") for client' do
          post(api_url("/controls"),
              clientwithnonode,
              :payload => input ) do |response|
                response.should look_like({
                  :status => 403
                })
              end
        end

      end

      context 'invalid input' do
        before :each do
          @input = input
        end
        after :each do
          @input = input
          delete(api_url("clients/client.opscode.piab"), platform.superuser)
          delete("#{platform.server}/clients/client.opscode.piab", platform.superuser)

          delete(api_url("clients/clientwithnonode.opscode.piab"), platform.superuser)
          delete("#{platform.server}/clients/clientwithnonode.opscode.piab", platform.superuser)
        end

        context 'missing run_id' do
          it 'returns a 400 ("Bad Request") for client missing run_id' do
            post(api_url("/controls"),
                client,
                :payload => @input.delete('run_id') ) do |response|
                  response.should look_like({
                    :status => 400
                  })
                end
          end
        end

        context 'missing node_name' do
          it 'returns a 400 ("Bad Request") for client missing node_name' do
            post(api_url("/controls"),
                client,
                :payload => @input.delete('node_name') ) do |response|
                  response.should look_like({
                    :status => 400
                  })
                end
          end
        end

        context 'missing control_groups' do
          it 'returns a 400 ("Bad Request") for client missing control_groups' do
            post(api_url("/controls"),
                client,
                :payload => @input.delete('control_groups') ) do |response|
                  response.should look_like({
                    :status => 400
                  })
                end
          end
        end

      end
    end # end 0.1.1 tests

  else # Pedant::Config.actions_enabled = false
    describe 'when actions is disabled' do
      it 'a POST to controls endpoint returns 410' do
        post(api_url("/controls"), platform.superuser, :payload => {} ) do |response|
              response.should look_like({
                :status => 410, :body_exact => {"error" => /\A/}
              })
            end
      end

      it 'a GET to controls endpoint returns 410' do
        get(api_url("/controls"), platform.superuser) do |response|
              response.should look_like({
                :status => 410, :body_exact => {"error" => /\A/}
              })
            end
      end
    end
  end
end
