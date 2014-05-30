require 'spec_helper'

describe ZendesksController do
  describe '#show' do
    context 'when zendesk is enabled' do
      context 'when not signed in' do
        it 'redirects to sign in'
      end

      context 'when signed in' do
        it 'does a JWT request to Zendesk'
      end
    end

    context 'when Zendesk is disabled' do
      it 'renders a 404' do
        controller.stub(:zendesk_enabled?).and_return false
        get 'show'
        expect(response.status).to eq 404
      end
    end
  end
end
