require 'spec_helper'

describe ZendesksController do
  describe '#show' do
    context 'when zendesk is enabled' do
      before :each do
        controller.stub(:zendesk_enabled?).and_return true
      end

      context 'when not signed in' do
        before :each do
          get 'show'
        end

        it 'redirects to sign in' do
          expect(response).to redirect_to '/id/signin'
        end

        it 'sets the return_to session variable' do
          expect(session[:return_to]).to eq '/id/zendesk'
        end

        context 'when a return_to param is present' do
          it 'includes it in the session variable' do
            get 'show', return_to: 'testreturnto'
            expect(session[:return_to]).to eq '/id/zendesk?return_to=testreturnto'
          end
        end
      end

      context 'when signed in' do
        before :each do
          controller.stub(:signed_in?).and_return true
          controller.stub(:current_user).and_return('testuser')
          controller.stub(:zendesk_sso_url).and_return('http://test')
        end

        it 'redirects to the zendesk SSO URL' do
          expect(controller).to receive(:zendesk_sso_url).with('testuser', 'testreturnto')
          get 'show', return_to: 'testreturnto'
        end
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
