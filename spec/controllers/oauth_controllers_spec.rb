require 'factory_girl'
require 'spec_helper'

describe Doorkeeper::ApplicationsController do
  describe '#index' do
    before do
      allow(Settings).to receive_message_chain(:doorkeeper, :administrators).and_return(['rainbowdash'])
      session[:username] = user.username if defined?(user)
      get :index
    end

    context 'as a signed-out user' do
      it 'should respond with a 404' do
        expect(response.status).to be(404)
      end
    end

    context 'as a signed-in non-administrator' do
      let(:user) { FactoryGirl.build(:user) }

      it 'should respond with a 404' do
        expect(response.status).to be(404)
      end
    end

    context 'as a signed-in administrator' do
      let(:user) { FactoryGirl.build(:administrator) }

      it 'should respond with a 200' do
        expect(response.status).to be(200)
      end
    end
  end
end
