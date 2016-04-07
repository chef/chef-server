require "veil/hasher/base"
require "securerandom"
require "bcrypt"

module Veil
  class Hasher
    class BCrypt < Base
      attr_reader :secret, :salt

      # Create a new BCrypt
      #
      # @param [Hash] opts
      #   a hash of options to pass to the constructor
      def initialize(opts = {})
        if opts[:secret] && opts[:salt]
          if ::BCrypt::Engine.valid_secret?(opts[:secret]) && ::BCrypt::Engine.valid_salt?(opts[:salt])
            @secret = opts.delete(:secret)
            @salt = opts.delete(:salt)
          elsif ::BCrypt::Engine.valid_secret?(opts[:secret])
            raise Veil::InvalidSalt.new("#{opts[:salt]} is not valid salt")
          else
            raise Veil::InvalidSecret.new("#{opts[:secret]} is not valid secret")
          end
        else
          @secret = SecureRandom.hex(512)
          @salt = ::BCrypt::Engine.generate_salt(opts[:cost] || 10)
        end
      end

      # Hash data with the stored secret and salt
      #
      # @param [String] data
      #   The service name and version to be encrypted with the shared key
      #
      # @return [String] SHA512 hex digest of hashed data
      def encrypt(data)
        hex_digest(::BCrypt::Engine.hash_secret(hex_digest([data, secret].join), salt))
      end

      # Return the instance as a Hash
      #
      # @return [Hash]
      def to_hash
        {
          type: self.class.name,
          secret: secret,
          salt: salt,
        }
      end
      alias_method :to_h, :to_hash
    end
  end
end
