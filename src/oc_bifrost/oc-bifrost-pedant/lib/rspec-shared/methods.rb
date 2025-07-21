# RSpecShared, written by Justin Ko <jko170@gmail.com>
# Original at https://github.com/justinko/rspec-shared

module RSpecShared
  module Methods
    def shared(name, &block)
      Thread.current[:rspec] ||= {}
      # Use a simpler key that doesn't rely on RSpec metadata
      cache_key = "#{self.class.name}_#{name}"

      define_method(name) do
        Thread.current[:rspec][cache_key] ||= instance_eval(&block)
      end
    end
  end
end
