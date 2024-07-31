require 'spec_helper'

describe ZendesksController do
  render_views

  describe '#show' do
    context 'when zendesk is enabled' do
      before :each do
        allow(controller).to receive(:zendesk_enabled?).and_return(true)
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
            get 'show', params: { return_to: 'testreturnto' }
            expect(session[:return_to]).to eq '/id/zendesk?return_to=testreturnto'
          end
        end
      end

      context 'when signed in' do
        let(:testuser) do
          User.new({
            :username => 'jimmy',
            :email => 'jim.kirk@federation-captains.org',
          })
        end
        before :each do
          allow(controller).to receive(:signed_in?).and_return(true)
          allow(controller).to receive(:current_user).and_return(testuser)
          allow(controller).to receive(:zendesk_sso_url).and_return('http://test')
        end

        it 'redirects to the zendesk SSO URL' do
          expect(controller).to receive(:zendesk_sso_url).with(testuser, 'testreturnto')
          get 'show', params: { return_to: 'testreturnto' }
        end
      end

      context 'when signed in' do
        let(:chefuser) do
          User.new({
            :username => 'jammy',
            :email => 'jam.kirk@chef.io',
          })
        end
        before :each do
          allow(controller).to receive(:signed_in?).and_return(true)
          allow(controller).to receive(:current_user).and_return(chefuser)
        end

        it 'renders json and a 403' do
          get 'show'
          expect(response.body).to eq("{\"message\":\"This account is not permitted for ZenDesk SSO\"}")
          expect(response.status).to eq 403
        end
      end
    end

    context 'when Zendesk is disabled' do
      it 'renders a 404' do
        allow(controller).to receive(:zendesk_enabled?).and_return(false)
        get 'show'
        expect(response.status).to eq 404
      end
    end
  end

  describe '#signout' do
    context 'when Zendesk is enabled' do
      before :each do
        allow(controller).to receive(:zendesk_enabled?).and_return(true)
      end

      it 'signs out' do
        expect(controller).to receive(:sign_out)
        get 'signout'
      end

      it 'redirects to the zendesk_path' do
        get 'signout'
        expect(response).to redirect_to zendesk_path
      end
    end

    context 'when Zendesk is disabled' do
      it 'renders a 404' do
        allow(controller).to receive(:zendesk_enabled?).and_return(false)
        get 'signout'
        expect(response.status).to eq 404
      end
    end
  end
end
