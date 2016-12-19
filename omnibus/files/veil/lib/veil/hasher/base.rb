require "openssl"
require "veil/exceptions"

module Veil
  class Hasher
    class Base

      # Hash the credential group, name and version with the stored secret and salt
      #
      # @param [String] group
      #   The service group name, eg: postgresql
      #
      # @param [String] name
      #   The credential name, eg: sql_password
      #
      # @param [Integer] version
      #   The Credential version, eg: 1
      #
      # @return [String] SHA512 hex digest of hashed data
      def encrypt(group, name, version)
        raise Veil::NotImplmented.new("#{caller[0]} has not implemented #encrypt")
      end

      # Return the instance as a Hash
      #
      # @return [Hash<Symbol,String>]
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
        OpenSSL::Digest::SHA512.hexdigest(data)
      end
    end
  end
end
