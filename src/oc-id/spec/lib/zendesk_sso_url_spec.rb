require 'cgi'
require 'uri'
require 'spec_helper'

describe ZendeskSSOURL do
  subject(:zendesk_sso_url) { described_class.new(user, return_to, settings) }
  let(:user) { double(email: 'test@test.com', first_name: 'Testy', last_name: 'Tester') }
  let(:return_to) { 'test/return/to' }
  let(:settings) { double(subdomain: 'testsubdomain', shared_secret: 'shhhhh') }

  describe '#to_s' do
    subject(:uri) { URI.parse(zendesk_sso_url.to_s) }

    it 'uses https' do
      expect(uri.scheme).to eq 'https'
    end

    it 'uses the zendesk subdomain' do
      expect(uri.host).to eq 'testsubdomain.zendesk.com'
    end

    it 'goes to the /access/jwt path' do
      expect(uri.path).to eq '/access/jwt'
    end

    it 'includes a jwt parameter' do
      expect(CGI.parse(uri.query)['jwt'][0].length).to_not eq 0
    end

    it 'includes the encoded return_to parameter' do
      expect(CGI.parse(uri.query)['return_to'][0]).to eq(
        'test%2Freturn%2Fto'
      )
    end

    context 'with no return_to' do
      let(:return_to) { nil }

      it 'does not have a return_to parameter' do
        expect(CGI.parse(uri.query)['return_to']).to eq []
      end
    end
  end
end
