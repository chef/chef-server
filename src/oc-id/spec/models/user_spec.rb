require 'spec_helper'

describe User do
  it { should respond_to(:username) }
  it { should respond_to(:first_name) }
  it { should respond_to(:last_name) }
  it { should respond_to(:middle_name) }
  it { should respond_to(:email) }
  it { should respond_to(:public_key) }
  it { should respond_to(:private_key) }
  it { should respond_to(:display_name) }
  it { should respond_to(:password) }

  let(:user_attrs) {
    {
      first_name: 'jimmy',
      last_name: 'jammy',
      username: 'jimmeh',
      display_name: 'jimmy jammy',
      email: 'jimmy@example.com',
      public_key: 'not a real public key lol'
    }
  }

  let(:jimmy) { User.new(user_attrs) }
  let(:chef) { double('chef', :put_rest => {'private_key' => 'my awesome private key'}) }

  before do
    allow(jimmy).to receive(:chef).and_return(chef)
    allow(User).to receive(:authenticate).and_return(true)
  end

  it 'should set attributes correctly' do
    expect(jimmy.first_name).to eql('jimmy')
    expect(jimmy.last_name).to eql('jammy')
    expect(jimmy.username).to eql('jimmeh')
    expect(jimmy.display_name).to eql('jimmy jammy')
    expect(jimmy.email).to eql('jimmy@example.com')
    expect(jimmy.public_key).to eql('not a real public key lol')
  end

  it 'should update users on the server' do
    new_attrs = user_attrs.merge('email' => 'jimmeh@example.com')
    expect(chef).to receive(:put_rest).with('users/jimmeh', new_attrs.stringify_keys).and_return({'private key' => 'my awesome private key'})
    jimmy.update_attributes('email' => 'jimmeh@example.com')
    expect(jimmy.email).to eql('jimmeh@example.com')
  end

  it 'can identify an admin' do
    allow(Settings).to receive_message_chain('doorkeeper.administrators') { ['jimmeh'] }
    expect(User.admin?(jimmy.username)).to eql(true)
  end

  it 'can handle find when supplied email' do
    allow(User).to receive(:new).and_return(jimmy)
    expect(chef).to receive(:get).with("users?#{ {email: jimmy.email }.to_query}")
                     .and_return([[jimmy.username]])
    expect(chef).to receive(:get_rest).with('users/jimmeh')
                     .and_return({'private key' => 'my awesome private key'})
    expect(User.find(jimmy.email)).to eql(jimmy)
  end

  it 'doesnt blow up when user is nil' do
    expect(User.find(nil)).to be_nil
  end

  describe 'updating a password' do
    fields = [:current_password, :new_password, :password_confirmation]
    fields.each do |f|
      it "populates an error if #{f.to_s} is blank" do
        params = fields.reject { |x| x == f }.reduce({}) do |result, field|
          result[field] = 'haha'
          result
        end
        jimmy.update_password(params)
        expect(jimmy.errors[f]).to_not be_nil
      end
    end

    it 'populates an error if the current password does not match what is entered' do
      expect(User).to receive(:authenticate).with('jimmeh', 'topsecret').and_return(false)
      jimmy.update_password(:current_password => 'topsecret')
      expect(jimmy.errors[:current_password]).to eql(['Current Password was not entered correctly.'])
    end

    it 'populates an error if the new password and its confirmation do not match' do
      jimmy.update_password(:new_password => 'haha', :password_confirmation => 'nope')
      expect(jimmy.errors[:base]).to eql(['The password you entered does not match the confirmation password.'])
    end

    it 'does not do the update if there are errors' do
      expect(jimmy).to_not receive(:update_attributes).with('password' => 'haha')
      jimmy.update_password(:new_password => 'haha', :password_confirmation => 'nope')
    end

    it 'only does the update if there are no errors' do
      expect(jimmy).to receive(:update_attributes).with('password' => 'haha')
      jimmy.update_password(:current_password => 'ohyes', :new_password => 'haha', :password_confirmation => 'haha')
    end
  end

  describe 'from_signed_request' do
    subject(:from_signed_request) do
      described_class.from_signed_request(request)
    end

    context 'when userid header is missing' do
      let(:request) { double(:headers => {}) }

      it { eq nil }
    end

    context 'when userid header is present' do
      let(:request) { double(:headers => { 'x-ops-userid' => 'testuser' }) }
      let(:authenticated?) { false }
      let(:verifier) { double(:authenticate_request => authenticated?) }
      let(:user) { double(:public_key => double) }

      before :each do
        allow(Mixlib::Authentication::SignatureVerification).to receive(:new)
          .and_return(verifier)
        allow(OpenSSL::PKey::RSA).to receive(:new).and_return(double)
        allow(User).to receive(:find).and_return(user)
      end

      context 'when the user does not exist' do
        let(:user) { nil }

        it { eq nil }
      end

      context 'when the signature is authenticated' do
        let(:authenticated?) { true }

        it { eq user }
      end

      context 'when the signature is not authenticated' do
        let(:authenticated?) { false }

        it { eq nil }
      end
    end
  end
end
