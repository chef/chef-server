# frozen_string_literal: true

require_relative '../../libraries/elasticsearch.rb'

describe Elasticsearch do
  describe '#memory_size' do
    context '4gb server' do
      let(:node) do
        {
          memory: { total: '4039772kB' }
        }
      end

      it 'Returns memory size for elasticsearch this should be no greater than 26GB and no less that 1GB' do
        expect(Elasticsearch.heap_size_default(node)).to eq(1024)
      end
    end

    context '64gb server' do
      let(:node) do
        {
          memory: { total: '67108864kB' }
        }
      end

      it 'Returns memory size for elasticsearch this should be no greater than 26GB and no less that 1GB' do
        expect(Elasticsearch.heap_size_default(node)).to eq(16_384)
      end
    end
    context '512gb server' do
      let(:node) do
        {
          memory: { total: '536870912kB' }
        }
      end

      it 'Returns memory size for elasticsearch this should be no greater than 26GB and no less that 1GB' do
        expect(Elasticsearch.heap_size_default(node)).to eq(26_624)
      end
    end
  end
end
