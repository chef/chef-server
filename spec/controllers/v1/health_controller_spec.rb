require 'spec_helper'

describe V1::HealthController do
  render_views

  describe 'GET #show' do
    before do
      hc = double(HealthCheck)
      allow(hc).to receive(:check).and_return(true)
      allow(hc).to receive(:status).and_return('ok')
      allow(hc).to receive(:erchef).and_return({})
      allow(hc).to receive(:postgres).and_return({})
      allow(HealthCheck).to receive(:new).and_return(hc)
      get :show, format: :json
    end

    it 'succeeds' do
      expect(response).to be_success
    end

    it 'has a health variable' do
      expect(assigns(:health)).to_not be_nil
    end

    it 'has some JSON output' do
      body = JSON.parse(response.body)
      expect(body.keys.size).to be > 0
    end
  end
end
