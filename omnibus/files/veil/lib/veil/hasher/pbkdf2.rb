require "veil/hasher/base"
require "securerandom"

module Veil
  class Hasher
    class PBKDF2 < Base
      attr_reader :secret, :salt, :iterations, :hash_function

      # Create a new PBKDF2
      #
      # @param [Hash] opts
      #   a hash of options to pass to the constructor
      def initialize(opts = {})
        @secret = opts[:secret] || SecureRandom.hex(512)
        @salt = opts[:salt] || SecureRandom.hex(128)
        @iterations = opts[:iterations] || 100_000
        @hash_function = OpenSSL::Digest.const_get((opts[:hash_function] || "SHA512")).new
      end

      # Hash data with the stored secret and salt
      #
      # @param [String] data
      #   The service name and version to be encrypted with the shared key
      #
      # @param [Hash] opts
      #   Optional parameter overrides
      #
      # @return [String] SHA512 hex digest of hashed data
      def encrypt(group, name, version)
        hex_digest(OpenSSL::PKCS5.pbkdf2_hmac(
          [secret, group, name, version].join,
          salt,
          iterations,
          hash_function.length,
          hash_function
        ))
      end

      # Return the instance as a Hash
      #
      # @return [Hash]
      def to_hash
        {
          type: self.class.name,
          secret: secret,
          salt: salt,
          iterations: iterations,
          hash_function: hash_function.class.name
        }
      end
      alias_method :to_h, :to_hash
    end
  end
end
