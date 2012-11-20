module Pedant
  class Organization
    attr_reader :name, :validator_key
    def initialize(name, key)
      @name = name
      raw = if key =~ /^-----BEGIN RSA PRIVATE KEY-----.*/
              key
            else
              IO.read(key).strip
            end
      @validator_key = OpenSSL::PKey::RSA.new(raw)
    end
  end
end
