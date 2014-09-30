require 'spec_helper'

describe User do
  it { should respond_to(:username) }
  it { should respond_to(:password) }
  it { should respond_to(:first_name) }
  it { should respond_to(:last_name) }
  it { should respond_to(:email) }
  it { should respond_to(:public_key) }

  describe 'authenticate_from_signed_request' do
    subject(:authenticate_from_signed_request) do
      described_class.authenticate_from_signed_request(request)
    end

    let(:request) { double }

    context 'when userid header is missing' do
      xit 'is nil' do
        expect(authenticate_from_signed_request).to be_nil
      end
    end

    context 'when userid header is present' do
      let(:request) { double(:headers => { 'x-ops-userid' => 'testuser' }) }

      context 'when signature is verified' do
        context 'when the signature is authenticated' do
          let(:user) { double(:username => 'testuser') }

          xit 'is the user object' do
            expect(authenticate_from_signed_request).to eq(user)
          end
        end

        context 'when the signature is not authenticated' do
          xit 'is nil' do
            expect(authenticate_from_signed_request).to be_nil
          end
        end
      end

      context 'when signature is not verified' do
        xit 'is nil' do
          expect(authenticate_from_signed_request).to be_nil
        end
      end
    end
  end
end
