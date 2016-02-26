module Pedant
  class Organization

    attr_reader :name, :validator_key
    attr_reader :validator

    # Create a new representation for an Organization Does not
    # actually create the organization on the server, but rather
    # stores information about the organization.
    #
    # The variety of acceptable validator parameters is just until we
    # fully migrate OHC / OPC to an Erchef-based Clients endpoint.
    #
    # @param name [String] The name of the organization
    #
    # @param validator [String, Pedant::Requestor] Either the textual
    #   content of a private key file, the name of a file containing a
    #   private key, or a proper {Pedant::Requestor} object.  If it is
    #   one of the first two, a Requestor will be created from it.
    #   Can also pass nil if no validator setup wanted.
    def initialize(name, validator)
      @name = name

      if validator.nil?
        @validator = nil
      elsif validator.is_a? Pedant::Requestor
        @validator = validator
        @validator_key = validator.signing_key
      else
        raw = if validator =~ /^-----BEGIN RSA PRIVATE KEY-----.*/
                validator
              else
                IO.read(validator).strip
              end
        @validator_key = OpenSSL::PKey::RSA.new(raw)
        @validator = Pedant::Client.new("#{@name}-validator", @validator_key)
      end
    end
  end
end
