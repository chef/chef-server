require 'spec_helper'

describe ProfilesController do
  let(:fake_user) {
    User.new({
      :username => 'jimmy',
      :first_name => 'jimmy',
      :last_name => 'jammy',
      :middle_name => 'jam',
      :email => 'jimmy@example.com',
      :public_key => 'fake public key',
      :display_name => 'jimmy jammy',
      :password => 'awesomefuntimes'
    })
  }

  let(:fake_chef) {
    double(:chef_api, :put_rest => {'private_key' => 'fake private key'})
  }

  before do
    allow(controller).to receive(:current_user).and_return(fake_user)
    allow(fake_user).to receive(:chef).and_return(fake_chef)
    allow(User).to receive(:authenticate).and_return(true)
  end

  describe 'GET #show' do
    before { get :show }

    it 'succeeds' do
      expect(response).to be_success
    end

    it 'renders the show template' do
      expect(response).to render_template('show')
    end

    it 'assigns the current user' do
      expect(assigns(:user).username).to eq('jimmy')
    end
  end

  describe 'PUT #update' do
    it 'redirects to the profile page if the update succeeded' do
      put :update, :user => {
        :first_name => 'sally',
        :last_name => 'solly'
      }

      expect(response).to redirect_to(profile_path)
    end

    it 'renders the show template if the update failed' do
      allow(fake_user).to receive(:chef).and_raise(StandardError)
      put :update, :user => {
        :first_name => 'sally',
        :last_name => 'solly'
      }

      expect(response).to render_template('show')
    end
  end

  describe 'PUT #change_password' do
    it 'redirects to the profile page if the update succeeded' do
      put :change_password, :current_password => 'haha',
                            :new_password => 'password',
                            :password_confirmation => 'password'

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
      expect(response).to be_success
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
