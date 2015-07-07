require 'spec_helper'

describe User do
  it { should respond_to(:username) }
  it { should respond_to(:password) }
  it { should respond_to(:first_name) }
  it { should respond_to(:last_name) }
  it { should respond_to(:email) }
  it { should respond_to(:public_key) }
  it { should respond_to(:display_name) }

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

  it 'should set attributes correctly' do
    expect(jimmy.first_name).to eql('jimmy')
    expect(jimmy.last_name).to eql('jammy')
    expect(jimmy.username).to eql('jimmeh')
    expect(jimmy.display_name).to eql('jimmy jammy')
    expect(jimmy.email).to eql('jimmy@example.com')
    expect(jimmy.public_key).to eql('not a real public key lol')
  end

  it 'should update users on the server' do
    chef = double('chef')
    new_attrs = user_attrs.merge('email' => 'jimmeh@example.com')
    allow(jimmy).to receive(:chef).and_return(chef)
    expect(chef).to receive(:put_rest).with('users/jimmeh', new_attrs.stringify_keys)
    jimmy.update_attributes('email' => 'jimmeh@example.com')
    expect(jimmy.email).to eql('jimmeh@example.com')
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

  context 'when retrieving all users on a Chef Server' do
    let(:chef) { double('chef') }

    before do
      allow_any_instance_of(ChefResource).to receive(:chef).and_return(chef)
    end

    it 'requests all users with verbose=true' do
      expect(chef).to receive(:get).with('users?verbose=true')
      User.all
    end
  end
end
