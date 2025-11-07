#!/usr/bin/env ruby
# Verifies our new util_api_url helper works in before/after(:all) scope
# and that EnvironmentUtil methods no longer depend on example-scoped api_url.

require "rspec"
require "pedant/config"
require_relative "../../lib/pedant/rspec/environment_util"

# Stub platform implementing api_url plus minimal requestor attributes
StubPlatform = Struct.new(:admin_user, :non_admin_user) do
  def api_url(path)
    "http://stub.example#{path}"
  end
  def test_org; OpenStruct.new(name: "stuborg"); end
  def bad_user; :bad_user; end
  def admin_client; :admin_client; end
  def non_admin_client; :non_admin_client; end
  def bad_client; :bad_client; end
  def validator_client; :validator_client; end
  def superuser; :superuser; end
  def superuser_key; "superuser_key"; end
  def webui_key; "webui_key"; end
end

RSpec.describe "util_api_url scope safety" do
  include Pedant::RSpec::EnvironmentUtil

  before(:all) do
    Pedant::Config.pedant_platform = StubPlatform.new(:admin_user, :normal_user)
    # Stub HTTP verbs used by helpers so we don't perform real requests
    allow(self).to receive(:post).and_return(:ok)
    allow(self).to receive(:delete).and_return(:ok)
    allow(self).to receive(:get).and_return(:ok)
  end

  it "composes URL outside example scope" do
    expect(util_api_url("/environments/test"))
      .to eq("http://stub.example/environments/test")
  end

  after(:all) do
    # Ensure delete_environment can call util_api_url here without WrongScopeError
    expect { delete_environment(:admin_user, "env_cleanup") }.not_to raise_error
  end
end
