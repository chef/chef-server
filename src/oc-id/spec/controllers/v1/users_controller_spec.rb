require 'spec_helper'

describe V1::UsersController do
  describe 'GET#show' do

    let(:app) { double('oauth_applications', id: '1') }

    let(:user) { FactoryGirl.build(:user) }

    let(:json) do
      { format: 'json', id: user.id, app_id: app.id}
    end

    it 'searches for the application' do
      expect(Doorkeeper::Application).to receive(:find_by_uid).with(app.id.to_s)
      get :show, json
    end

    context 'when the app_id is valid' do
      before do
        allow(Doorkeeper::Application).to receive(:find_by_uid).with(app.id).and_return(app)
      end

      it 'searches for the user' do
        expect(User).to receive(:find).with(user.username)
        get :show, json
      end

      context 'when the user is valid' do
        before do
          allow(User).to receive(:find).and_return(user)
        end

        it 'returns the public user' do
          json = { format: 'json', id: user.id, app_id: app.id}

          get :show, json

          expect(response.body).to include(user.username, user.first_name, user.last_name)
        end
      end
    end
  end
end
