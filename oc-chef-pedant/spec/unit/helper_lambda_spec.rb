# Minimal unit tests for converted helper lambdas.
# These validate behavior of normalize_run_list, update_object, and internal_api_url
# after converting them to let-with-lambda form.

require 'rspec'

# Ensure oc-chef-pedant lib directory is on the load path so that
# `require "rspec-shared/methods"` inside common.rb can resolve.
$LOAD_PATH.unshift File.expand_path('../../lib', __FILE__)


class FakePlatform
  def api_url(fragment)
    "http://api.example#{fragment}"
  end

  def internal_api_url(fragment)
    "http://internal.example#{fragment}"
  end
end

# After loading real Pedant classes we patch pedant_platform accessor

# Require the actual helper modules under test
require_relative '../../lib/pedant/rspec/common'
require_relative '../../lib/pedant/rspec/internal_api'

module Pedant
  class Config
    def self.pedant_platform
      @pedant_platform ||= FakePlatform.new
    end
  end
end

RSpec.describe 'Helper lambda conversions' do
  # Sanity: normalize_run_list should be a lambda returned by let
  it 'exposes normalize_run_list as a callable' do
    expect(normalize_run_list).to respond_to(:call)
  end
  include Pedant::RSpec::Common
  include Pedant::RSpec::InternalAPI

  describe 'normalize_run_list.call' do
    it 'wraps bare entries as recipes and preserves explicit recipe/role entries' do
      input = ['cookbook::default', 'recipe[apache]', 'role[web]']
      result = normalize_run_list.call(input)
      expect(result).to contain_exactly('recipe[cookbook::default]', 'recipe[apache]', 'role[web]')
    end

    it 'deduplicates entries after normalization' do
      input = ['cookbook::default', 'cookbook::default', 'recipe[cookbook::default]']
      result = normalize_run_list.call(input)
      expect(result).to eq(['recipe[cookbook::default]'])
    end
  end

  describe 'update_object.call' do
    it 'merges updates and applies :DELETE sentinel removals' do
      base = { 'a' => 1, 'b' => 2, 'c' => 3 }
      updates = { 'b' => 99, 'c' => :DELETE, 'd' => 4 }
      result = update_object.call(base, updates)
      expect(result).to eq({ 'a' => 1, 'b' => 99, 'd' => 4 })
    end

    it 'handles multiple deletions and no additions' do
      base = { 'x' => 1, 'y' => 2 }
      updates = { 'x' => :DELETE, 'y' => :DELETE }
      result = update_object.call(base, updates)
      expect(result).to eq({})
    end
  end

  describe 'internal_api_url.call' do
    it 'delegates to platform internal_api_url' do
      expect(internal_api_url.call('/nodes')).to eq('http://internal.example/nodes')
    end
  end
end
