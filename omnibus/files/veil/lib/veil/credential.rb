module Veil
  class Credential
    class << self
      def create(hash = {})
        new(hash)
      end
    end

    attr_reader :version, :value, :name, :length, :group
    alias_method :credential, :value

    def initialize(opts = {})
      validate_opts!(opts)
      @name = opts[:name]
      @version = opts[:version] || 0
      @group = opts[:group] || nil
      @value = opts[:value]
      @length = opts[:length] || value.length
    end

    def eql?(other)
      (@name == other.name) &&
        (@group == other.group) &&
        (@value == other.value) &&
        (@version == other.version)
    end

    def hash
      [@name, @group, @value, @version].hash
    end

    def rotate(hasher)
      raise InvalidHasher.new("You must supply a valid hasher to rotate a credential") unless hasher.respond_to?(:encrypt)
      @version += 1
      @value = hasher.encrypt(group, name, version)[0...length]
    end

    def to_hash
      {
        type: self.class.name,
        name: name,
        group: group,
        value: value,
        version: version,
        length: length
      }
    end
    alias_method :to_h, :to_hash

    private

    def validate_opts!(opts)
      raise ArgumentError, "You must provide a credential name" unless opts[:name]
      raise ArgumentError, "You must provide a credential value" unless opts[:value]
    end
  end
end
