require 'spec_helper'
require 'timecop'

describe PasswordResetsController do
  describe 'GET #new' do
    before { get :new }

    it 'succeeds' do
      expect(response).to have_http_status(:success)
    end

    it 'renders the new template' do
      expect(response).to render_template('new')
    end
  end

  describe 'POST #create' do
    describe 'no username' do
      before { post :create }

      it 'shows a flash message prompting for a username' do
        expect(flash[:alert]).to eql('Please enter a username or email address.')
      end

      it 'returns a 422' do
        expect(response.status).to eql(422)
      end

      it 'renders the new template' do
        expect(response).to render_template('new')
      end
    end

    describe 'user not found' do
      before do
        allow(User).to receive(:find).and_return(nil)
        post :create, params: { username: 'jimmy' }
      end

      it 'shows a flash message that is informative but not revealing' do
        expect(flash[:notice]).to match(/entered exists.*email shortly/)
      end

      it 'returns a 200 which is a lie but better for security' do
        expect(response.status).to eql(200)
      end

      it 'renders the new template' do
        expect(response).to render_template('new')
      end
    end

    describe 'nebulous errors talking to the chef server' do
      before do
        user = double('user', username: 'jimmy')
        allow(User).to receive(:find).and_return(user)
      end

      describe 'chef server returns a 404' do
        before do
          r = Net::HTTPNotFound.new('1.0', '404', 'Not Found')
          e = Net::HTTPServerException.new('fake exception', r)
          allow(PasswordResetMailer).to receive(:password_reset).and_raise(e)
          post :create, params: { username: 'jimmy' }
        end

        it 'shows a flash message that is informative but not revealing' do
          expect(flash[:notice]).to match(/entered exists.*email shortly/)
        end

        it 'returns a 200 which is a lie but better for security' do
          expect(response.status).to eql(200)
        end

        it 'renders the new template' do
          expect(response).to render_template('new')
        end
      end

      describe 'chef server returns a 406' do
        before do
          r = Net::HTTPNotAcceptable.new('1.0', '406', 'Not Acceptable')
          e = Net::HTTPServerException.new('fake exception', r)
          allow(PasswordResetMailer).to receive(:password_reset).and_raise(e)
          post :create, params: { username: 'jimmy' }
        end

        it 'shows a flash message explaining the problem' do
          expect(flash[:alert]).to match(/not a valid username/)
        end

        it 'returns a 403' do
          expect(response.status).to eql(403)
        end

        it 'renders the new template' do
          expect(response).to render_template('new')
        end
      end
    end

    describe 'valid params' do
      before do
        user = double(
          'user',
          username: 'jimmy',
          email: 'jimmy@example.com',
          first_name: 'Jimmy',
          last_name: 'Smith'
        )
        allow(User).to receive(:find).and_return(user)
        post :create, params: { username: 'jimmy' }
      end

      it 'sends an email' do
        expect(ActionMailer::Base.deliveries.size).to eql(1)
      end

      it 'shows a flash message explaining an email was sent' do
        expect(flash[:notice]).to match(/entered exists.*email shortly/)
      end

      it 'succeeds' do
        expect(response).to have_http_status(:success)
      end

      it 'renders the new template' do
        expect(response).to render_template('new')
      end
    end
  end

  describe 'GET #show' do
    Timecop.freeze(Time.utc(2015, 2, 19, 12, 12, 12)) do
      let!(:name) { 'jimmy' }
      let!(:email) { 'jim@federation-captains.org' }
      let!(:expires) { 1.day.from_now.to_i }
      let!(:signature) { Signature.new(name, email, expires, Settings.secret_key_base) }

      it 'succeeds when given a valid signature' do
        get :show, params: { expires: expires, signature: signature, username: name, email: email }
        expect(response).to have_http_status(:success)
      end

      describe 'invalid params' do
        it 'requires an email' do
          get :show, params: { expires: expires, signature: signature, username: name }
          expect(response).to redirect_to(action: 'new')
          expect(flash[:alert]).to match /invalid signature/
        end

        it 'requires a username' do
          get :show, params: { expires: expires, signature: signature, email: email }
          expect(response).to redirect_to(action: 'new')
          expect(flash[:alert]).to match /invalid signature/
        end

        it 'requires an expiration date' do
          get :show, params: { username: name, signature: signature, email: email }
          expect(response).to redirect_to(action: 'new')
          expect(flash[:alert]).to match /invalid signature/
        end

        it 'requires a signature' do
          get :show, params: { expires: expires, username: name, email: email }
          expect(response).to redirect_to(action: 'new')
          expect(flash[:alert]).to match /invalid signature/
        end

        it 'requires a valid signature' do
          get :show, params: { expires: expires, signature: 'haha', username: name, email: email }
          expect(response).to redirect_to(action: 'new')
          expect(flash[:alert]).to match /invalid signature/
        end
      end
    end
  end

  describe 'PUT #update' do
    describe 'no password' do
      before { put :update }

      it 'shows a flash message explaining what went wrong' do
        expect(flash[:alert]).to eql('Password must not be blank.')
      end

      it 'renders the show template' do
        expect(response).to render_template('show')
      end

      it 'returns a 403' do
        expect(response.status).to eql(403)
      end
    end

    describe 'invalid signature' do
      before { put :update, params: { password: 'haha', signature: 'no' } }

      it 'shows a flash message explaining the problem' do
        expect(flash[:alert]).to match(/invalid signature/)
      end

      it 'renders the show template' do
        expect(response).to render_template('show')
      end

      it 'returns a 403' do
        expect(response.status).to eql(403)
      end
    end

    describe 'valid params' do
      let(:name) { 'jimmy' }
      let(:email) { 'jim@federation-captains.org' }
      let(:expires) { 1.day.from_now.to_i }
      let(:signature) { Signature.new(name, email, expires, Settings.secret_key_base) }

      Timecop.freeze(Time.utc(2015, 2, 19, 12, 12, 12)) do
        let!(:expires) { 1.day.from_now.to_i }

        describe 'nebulous errors talking to the chef server' do
          describe 'chef server returns a 404' do
            before do
              r = Net::HTTPNotFound.new('1.0', '404', 'Not Found')
              e = Net::HTTPServerException.new('fake exception', r)
              allow(User).to receive(:find).and_raise(e)
              put :update, params: { password: 'haha', signature: signature, expires: expires, username: name, email: email }
            end

            it 'shows a flash message that is informative but not revealing' do
              expect(flash[:notice]).to match(/entered exists.*email shortly/)
            end

            it 'redirects to new' do
              expect(response).to redirect_to(action: 'new')
            end
          end

          describe 'chef server returns a 400' do
            before do
              r = Net::HTTPBadRequest.new('1.0', '400', 'Bad Request')
              e = Net::HTTPServerException.new('fake exception', r)
              allow(User).to receive(:find).and_raise(e)
              allow(controller).to receive(:error_from_json).and_return({
                'error' => 'oh no!'
              })
              put :update, params: { password: 'haha', signature: signature, expires: expires, username: name, email: email }
            end

            it 'shows a flash message explaining the problem' do
              expect(flash[:alert]).to_not be_nil
            end

            it 'returns a 400' do
              expect(response.status).to eql(400)
            end

            it 'renders the show template' do
              expect(response).to render_template('show')
            end
          end

          describe 'email mismatch' do
            let!(:user) do
              User.new(
                first_name: 'Jimmy',
                last_name: 'Kirk',
                email: 'evil-kirk@alternate-timeline.com',
                username: name,
                display_name: 'Jimmy Kirk'
              )
            end

            before do
              allow(User).to receive(:find).with(name).and_return(user)
              put :update, params: { password: 'haha', signature: signature, expires: expires, username: name, email: email }
            end

            it 'shows a flash message explaining the problem' do
              expect(flash[:alert]).to match(/invalid signature/)
            end

            it 'renders the show template' do
              expect(response).to render_template('show')
            end

            it 'returns a 403' do
              expect(response.status).to eql(403)
            end
          end
        end
      end

      describe 'no errors' do
        let!(:user) {
          User.new(
            first_name: 'Jimmy',
            last_name: 'Smith',
            email: email,
            username: name,
            display_name: 'Jimmy Smith'
          )
        }
        before do
          allow(User).to receive(:find).with(name).and_return(user)
          allow(user).to receive_message_chain(:chef, :put_rest).and_return({'private_key' => 'my awesome private key'})
          put :update, params: { password: 'haha', signature: signature, expires: expires, username: name, email: email }
        end

        it 'updates the user\'s password' do
          expect(user.password).to eql('haha')
        end

        it 'stores the username in the session' do
          expect(session[:username]).to eql(name)
        end

        it 'redirects to the signin screen' do
          expect(response).to redirect_to(signin_path)
        end
      end
    end
  end
end
