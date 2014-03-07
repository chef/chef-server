# encoding: utf-8

require 'spec_helper'

describe OmniAuth::Strategies::Chef do
  let (:application) { lambda { |environment| [404, { env: environment }, ['…']] } }

  let (:strategy) { OmniAuth::Strategies::Chef.new(application) }


  describe '.request_phase' do

  end

  describe '.callback_phase' do
    context 'with valid credentials' do

    end

    context 'with invalid credentials' do

    end
  end

  describe '.authenticated?' do
    let! (:user) { strategy.authenticate('applejack', 'applejack') }

    it { expect(strategy.authenticated?(user)).to eq(true) }
  end

  describe '.chef' do
    it { expect(strategy.authenticate('applejack', 'applejack')['status']).to eq('linked') }
  end

  describe '.endpoint' do
    it { expect(strategy.endpoint).to eq('https://api.opscode.piab') }
  end

  describe '.key' do
    it { expect(strategy.key).to include('BEGIN RSA PRIVATE KEY') }
  end

  describe '.request_source' do
    it { expect(strategy.request_source).to include({ "x-ops-request-source" => "web" }) }
  end
end
