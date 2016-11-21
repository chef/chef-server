module Veil
  module Utils
    class << self
      def symbolize_keys(hash)
        new_hash = {}
        hash.keys.each { |k| new_hash[k.to_sym] = hash.delete(k) }
        new_hash
      end

      def symbolize_keys!(hash)
        hash = symbolize_keys(hash)
      end

      def stringify_keys(hash)
        new_hash = {}
        hash.keys.each { |k| new_hash[k.to_s] = hash.delete(k) }
        new_hash
      end

      def stringify_keys!(hash)
        hash = stringify_keys(hash)
      end
    end
  end
end
