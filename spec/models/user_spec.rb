require 'spec_helper'

describe User do
  it { should respond_to(:username) }
  it { should respond_to(:password) }
  it { should respond_to(:first_name) }
  it { should respond_to(:last_name) }
  it { should respond_to(:email) }
  it { should respond_to(:public_key) }

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
