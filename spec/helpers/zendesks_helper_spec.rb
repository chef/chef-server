require 'spec_helper'

describe ZendesksHelper do
  describe 'zendesk_enabled?' do
    subject(:zendesk_enabled?) { helper.zendesk_enabled? }

    context 'when both Settings.zendesk.shared_secret and Settings.zendesk.subdomain are present' do
      it 'is true' do
        allow(Settings).to receive(:zendesk).and_return(double(shared_secret: 'test', subdomain: 'test'))
        expect(zendesk_enabled?).to eq true
      end
    end

    context 'when Settings.zendesk.shared_secret is missing' do
      it 'is false' do
        allow(Settings).to receive(:zendesk).and_return(double(shared_secret: nil, subdomain: 'test'))
        expect(zendesk_enabled?).to eq false
      end
    end

    context 'when Settings.zendesk.subdomain is missing' do
      it 'is false' do
        allow(Settings).to receive(:zendesk).and_return(double(shared_secret: 'test', subdomain: nil))
        expect(zendesk_enabled?).to eq false
      end
    end

    context 'when both Settings.zendesk.shared_secret and Settings.zendesk.subdomain are missing' do
      it 'is false' do
        allow(Settings).to receive(:zendesk).and_return(double(shared_secret: nil, subdomain: nil))
        expect(zendesk_enabled?).to eq false
      end
    end
  end
end
