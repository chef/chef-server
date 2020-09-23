require 'spec_helper'

describe V1::HealthController do
  render_views

  describe 'GET #show' do
    let!(:hc) { double(HealthCheck) }

    before do
      allow(hc).to receive(:check).and_return(true)
      allow(hc).to receive(:erchef).and_return({})
      allow(hc).to receive(:postgres).and_return({})
      allow(HealthCheck).to receive(:new).and_return(hc)
    end

    context 'everything is working' do
      before do
        allow(hc).to receive(:status).and_return('ok')
        allow(hc).to receive(:ok?).and_return(true)
        get :show, format: :json
      end

      it 'succeeds' do
        expect(response).to have_http_status(:success)
      end

      it 'has a health variable' do
        expect(assigns(:health)).to_not be_nil
      end

      it 'has some JSON output' do
        body = JSON.parse(response.body)
        expect(body.keys.size).to be > 0
      end
    end

    context 'something went wrong' do
      before do
        allow(hc).to receive(:status).and_return('not ok')
        allow(hc).to receive(:ok?).and_return(false)
        get :show, format: :json
      end

      it 'returns a 503' do
        expect(response.status).to eql(503)
      end
    end
  end
end
