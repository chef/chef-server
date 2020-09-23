require 'spec_helper'

describe ProfilesController do
  let(:username) { 'jimmy' }
  let(:email) { 'jim.kirk@federation-captains.org' }
  let(:user1) do
    User.new({
      :username => username,
      :first_name => 'jimmy',
      :last_name => 'jammy',
      :middle_name => 'jam',
      :email => email,
      :public_key => 'fake public key',
      :display_name => 'jimmy jammy',
      :password => 'awesomefuntimes'
    })
  end
  let(:user2) do
    User.new({
      :username => 'spock',
      :first_name => 'Spock',
      :last_name => 'Son Of Sarek',
      :middle_name => '',
      :email => 'spock@vulcan.net',
      :public_key => 'fake public key',
      :display_name => 'Spock',
      :password => 'highlylogical'
    })
  end

  let(:logged_in_user) { user1 }
  let(:fake_chef) {
    double(:chef_api, :put_rest => {'private_key' => 'fake private key'})
  }

  before do
    allow(User).to receive(:find).with(user1.username).and_return(user1)
    allow(User).to receive(:find).with(user2.username).and_return(user2)
    allow(user1).to receive(:chef).and_return(fake_chef)
    allow(user2).to receive(:chef).and_return(fake_chef)
    allow(controller).to receive(:current_user).and_return(logged_in_user)

    allow(User).to receive(:authenticate).and_return(true)
  end

  describe 'GET #show' do
    before { get :show }

    it 'succeeds' do
      expect(response).to have_http_status(:success)
    end

    it 'renders the show template' do
      expect(response).to render_template('show')
    end

    it 'assigns the current user' do
      expect(assigns(:user).username).to eq(username)
    end
  end

  describe 'PUT #update' do
    let(:put_user) do
      {
        first_name: 'sally',
        last_name: 'solly'
      }
    end

    it 'redirects to the profile page if the update succeeded' do
      put :update, params: { user: put_user }

      expect(response).to redirect_to(profile_path)
    end

    it 'renders the show template if the update failed' do
      allow(logged_in_user).to receive(:chef).and_raise(StandardError)
      put :update, params: { user: put_user }

      expect(response).to render_template('show')
    end
  end

  describe 'GET #change_email' do
    let(:new_email) { 'new-email@somewhere.org' }
    let(:signature) { Signature.new(username, email, expires, Settings.secret_key_base, new_email) }
    let(:expires) { 1.day.from_now.to_i }

    describe 'when logged in as user' do
      describe 'invalid params' do
        it 'requires a username' do
          get :change_email, params: { email: new_email, signature: signature, expires: expires }
          expect(response).to render_template('show')
          expect(flash[:alert]).to match /not a valid username/
        end

        it 'requires a new email' do
          get :change_email, params: { username: username, signature: signature, expires: expires }
          expect(response).to render_template('show')
          expect(flash[:alert]).to match /invalid signature/
        end

        it 'requires an expiration date' do
          get :change_email, params: { username: username, email: new_email, signature: signature }
          expect(response).to render_template('show')
          expect(flash[:alert]).to match /invalid signature/
        end

        it 'requires a signature' do
          get :change_email, params: { username: username, email: new_email, expires: expires }
          expect(response).to render_template('show')
          expect(flash[:alert]).to match /invalid signature/
        end

        it 'requires a valid signature - signing the new email address' do
          get :change_email, params: { username: username, email: new_email, expires: expires, signature: 'foo' }
          expect(response).to render_template('show')
          expect(flash[:alert]).to match /invalid signature/
        end
      end

      describe 'expired link' do
        let(:expires) { 1.day.ago.to_i }

        it 'rejects the link' do
          get :change_email, params: { username: username, email: new_email, expires: expires, signature: signature }
          expect(response).to render_template('show')
          expect(flash[:alert]).to match /invalid signature/
        end

      end

      describe 'link with stale email' do
        let(:old_email) { 'myfirstemail@email.org' }
        let(:signature) { Signature.new(username, old_email, expires, Settings.secret_key_base, new_email) }

        it 'rejects the link' do
          get :change_email, params: { username: username, email: new_email, expires: expires, signature: signature }
          expect(response).to render_template('show')
          expect(flash[:alert]).to match /invalid signature/
        end
      end

      describe 'valid params' do
        before do
          get :change_email, params: { username: username, email: new_email, expires: expires, signature: signature }
        end

        it 'redirects to the profile page' do
          expect(response).to redirect_to(profile_path)
        end

        it 'updates the email address' do
          expect(logged_in_user.email).to eql(new_email)
        end
      end
    end

    describe 'when not logged in' do
      let(:logged_in_user) { nil }

      describe 'invalid params' do
        it 'requires a username' do
          get :change_email, params: { email: new_email, signature: signature, expires: expires }
          expect(response).to redirect_to(signin_path)
        end

        it 'requires a new email' do
          get :change_email, params: { username: username, signature: signature, expires: expires }
          expect(response).to redirect_to(signin_path)
        end

        it 'requires an expiration date' do
          get :change_email, params: { username: username, email: new_email, signature: signature }
          expect(response).to redirect_to(signin_path)
        end

        it 'requires a signature' do
          get :change_email, params: { username: username, email: new_email, expires: expires }
          expect(response).to redirect_to(signin_path)
        end

        it 'requires a valid signature - signing the new email address' do
          get :change_email, params: { username: username, email: new_email, expires: expires, signature: 'foo' }
          expect(response).to redirect_to(signin_path)
        end
      end

      describe 'expired link' do
        let(:expires) { 1.day.ago.to_i }

        it 'rejects the link' do
          get :change_email, params: { username: username, email: new_email, expires: expires, signature: signature }
          expect(response).to redirect_to(signin_path)
        end

      end

      describe 'link with stale email' do
        let(:signature) { Signature.new(username, 'myfirstemail@email.org', expires, Settings.secret_key_base, new_email) }

        it 'rejects the link' do
          get :change_email, params: { username: username, email: new_email, expires: expires, signature: signature }
          expect(response).to redirect_to(signin_path)
        end
      end

      describe 'valid params' do
        before do
          get :change_email, params: { username: username, email: new_email, expires: expires, signature: signature }
        end

        it 'redirects to the signin page' do
          expect(response).to redirect_to(signin_path)
        end

        it 'updates the email address' do
          expect(user1.email).to eql(new_email)
        end
      end
    end

    describe 'when logged in as someone else' do
      let(:logged_in_user) { user2 }

      describe 'link with stale email' do
        let(:signature) { Signature.new(username, user2.email, expires, Settings.secret_key_base, new_email) }

        it 'rejects the link' do
          get :change_email, params: { username: username, email: new_email, expires: expires, signature: signature }
          expect(response).to render_template('show')
          expect(flash[:alert]).to match /invalid signature/
        end
      end

      describe 'valid params' do
        before do
          get :change_email, params: { username: username, email: new_email, expires: expires, signature: signature }
        end

        it 'redirects to the profile page' do
          expect(response).to redirect_to(profile_path)
        end

        it 'updates the email address' do
          expect(user1.email).to eql(new_email)
        end
      end
    end
  end

  describe 'PUT #change_password' do
    it 'redirects to the profile page if the update succeeded' do
      put :change_password, params: { :current_password => 'haha',
                              :new_password => 'password',
                              :password_confirmation => 'password'
                            }

      expect(response).to redirect_to(profile_path)
    end

    context 'failure' do
      before do
        put :change_password
      end

      it 'renders the show template' do
        expect(response).to render_template('show')
      end

      it 'returns a 403' do
        expect(response.status).to eql(403)
      end

      it 'shows an alert' do
        expect(flash[:alert]).to_not be_nil
      end
    end
  end

  describe 'POST #regen_key' do
    before do
      post :regen_key
    end

    it 'succeeds' do
      expect(response).to have_http_status(:success)
    end

    context 'pem file' do
      it 'names the pem file after the user' do
        expect(response.headers['Content-Disposition']).to match(/jimmy\.pem/)
      end

      it 'sets the content type to one that is appropriate for pem keys' do
        expect(response.content_type).to eql('application/pem-keys')
      end

      it 'sends the pem file as an attachment' do
        expect(response.headers['Content-Disposition']).to match(/attachment/)
      end

      it 'contains the correct private key' do
        expect(response.body).to eql('fake private key')
      end
    end
  end
end
