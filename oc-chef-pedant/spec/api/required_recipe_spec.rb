# Author:: Ryan Cragun <me@ryan.ec>
# Copyright:: Copyright (c) 2017 Chef, Inc.

require "pedant/rspec/common"

describe "Required Recipe Endpoint", :required_recipe do
  if Pedant::Config.required_recipe_enabled
    describe "when required_recipe is enabled" do
      describe "with a valid client" do
        it "POST to /required_recipe returns 405" do
          post(api_url("/required_recipe"), normal_client, payload: {}) do |response|
            response.should look_like(status: 405)
          end
        end

        it "GET to /required_recipe returns 200" do
          get(api_url("/required_recipe"), normal_client) do |response|
            response.should look_like(status: 200)
          end
        end
      end

      describe "with an invalid client" do
        it "POST to /required_recipe returns 405" do
          post(api_url("/required_recipe"), platform.bad_client, payload: {}) do |response|
            response.should look_like(status: 405)
          end
        end

        it "GET to /required_recipe returns 401" do
          get(api_url("/required_recipe"), platform.bad_client, {}) do |response|
            response.should look_like(status: 401)
          end
        end
      end

      describe "with a bad request" do
        it "POST to /required_recipe returns 405" do
          do_request(:POST, api_url("/required_recipe"), {}, payload: {}) do |response|
            response.should look_like(status: 405)
          end
        end

        it "GET to /required_recipe returns 400" do
          do_request(:GET, api_url("/required_recipe"), {}, {}) do |response|
            response.should look_like(status: 400)
          end
        end
      end
    end
  else
    describe "when required_recipe is disabled" do
      describe "with a valid client" do
        it "POST to /required_recipe returns 404" do
          post(api_url("/required_recipe"), normal_client, payload: {}) do |response|
            response.should look_like(status: 404)
          end
        end

        it "GET to /required_recipe returns 404" do
          get(api_url("/required_recipe"), normal_client, {}) do |response|
            response.should look_like(status: 404)
          end
        end
      end

      describe "with an invalid client" do
        it "POST to /required_recipe returns 404" do
          post(api_url("/required_recipe"), platform.bad_client, payload: {}) do |response|
            response.should look_like(status: 404)
          end
        end

        it "GET to /required_recipe returns 404" do
          get(api_url("/required_recipe"), platform.bad_client, {}) do |response|
            response.should look_like(status: 404)
          end
        end
      end

      describe "with a bad request" do
        it "POST to /required_recipe returns 404" do
          do_request(:POST, api_url("/required_recipe"), {}, {}) do |response|
            response.should look_like(status: 404)
          end
        end

        it "GET to /required_recipe returns 404" do
          do_request(:GET, api_url("/required_recipe"), {}, {}) do |response|
            response.should look_like(status: 404)
          end
        end
      end
    end
  end
end
