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

  describe 'GET#all' do
    let(:app) { double('oauth_applications', id: '1') }

    let(:user) { FactoryGirl.build(:user) }

    let(:json) do
      { format: 'json', app_id: app.id}
    end

    let(:sample_users) do
      {
        "some_user"=> {
          "email"=> user.email,
          "first_name" => user.first_name,
          "last_name" => user.last_name
        }
      }
    end

    let(:token) { double acceptable?: true }

    before do
      # Stubs doorkeeper authentication
      allow(controller).to receive(:doorkeeper_token).and_return(token)

      allow(User).to receive(:all).and_return(sample_users)
    end

    it 'searches for the app' do
      expect(Doorkeeper::Application).to receive(:find_by_uid).with(app.id.to_s)
      get :all_users, json
    end

    context 'when the app is valid' do
      before do
        allow(Doorkeeper::Application).to receive(:find_by_uid).with(app.id.to_s).and_return(app)
      end

      it 'finds all users' do
        expect(User).to receive(:all).and_return(sample_users)
        get :all_users, json
      end

      it 'returns all users' do
        get :all_users, json
        expect(response.body).to include("some_user", user.email, user.first_name, user.last_name)
      end
    end

    context 'when the app is not valid' do
      before do
        allow(Doorkeeper::Application).to receive(:find_by_uid).with(app.id.to_s).and_return(nil)
      end

      it 'does not find all users' do
        expect(User).to_not receive(:all)
        get :all_users, json
      end

      it 'does not return all users' do
        get :all_users, json
        expect(response.body).to_not include("some_user", user.email, user.first_name, user.last_name)
      end
    end
  end
end
