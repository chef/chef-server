module DVM
  class Dep
    include DVM::Tools
    attr_reader :name
    def initialize(name)
      # Make it explicit, but a subclass is expected to lazy-load this check status
      @available = nil
    end
    def loaded?
      false
    end
    def available?
      @available
    end
    def unload

    end
    def load

    end

  end
end

