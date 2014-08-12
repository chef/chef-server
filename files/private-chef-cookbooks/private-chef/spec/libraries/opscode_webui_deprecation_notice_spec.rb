require_relative '../../libraries/opscode_webui_deprecation_notice'

describe OpscodeWebuiDeprecationNotice do
  subject(:opscode_webui_deprecation_notice) { described_class.new(attributes) }

  describe '#applicable?' do
    subject(:applicable?) { opscode_webui_deprecation_notice.applicable? }

    context 'when opscode_webui attributes are set' do
      let(:attributes) { { 'test' => 123 } }

      it 'is true' do
        expect(applicable?).to eq true
      end
    end

    context 'when opscode_webui attributes are not set' do
      let(:attributes) { {} }

      it 'is false' do
        expect(applicable?).to eq false
      end
    end
  end

  describe '#message' do
    let(:attributes) { { 'abc' => '123', 'def' => '456' } }

    it 'shows the attributes' do
      expect(opscode_webui_deprecation_notice.message).to include('abc', 'def')
    end
  end
end
