require "openssl"
require "veil/exceptions"

module Veil
  class Hasher
    class Base
      def encrypt(data, opts = {})
        raise Veil::NotImplmented.new("#{caller[0]} has not implemented #encrypt")
      end

      def to_hash
        raise Veil::NotImplmented.new("#{caller[0]} has not implemented #to_hash")
      end

      private

      # Create a SHA512 hex digest
      #
      # @param [String] data
      #   Data to digest
      #
      # @return [String]
      def hex_digest(data)
        OpenSSL::HMAC.new(data, OpenSSL::Digest::SHA512.new).to_s
      end
    end
  end
end
