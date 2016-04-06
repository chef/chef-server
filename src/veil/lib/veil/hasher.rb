require "veil/utils"
require "veil/hasher/base"
require "veil/hasher/bcrypt"
require "veil/hasher/pbkdf2"

module Veil
  class Hasher
    DEFAULT_OPTIONS = {
      type: "Veil::Hasher::PBKDF2",
      iterations: 10_000,
      hash_function: "SHA512"
    }

    class << self
      #
      # Create a new Hasher instance
      #
      # @param opts Hash<Symbol> a hash of options to pass to the constructor
      #
      # @example Veil::Hasher.create(type: "BCrypt", cost: 10)
      # @example Veil::Hasher.create(type: "PBKDF2", iterations: 1000, hash_function: "SHA256")
      #
      def create(opts = {})
        opts = Veil::Utils.symbolize_keys(DEFAULT_OPTIONS.merge(opts))
        const_get(opts[:type]).new(opts)
      end
    end
  end
end
