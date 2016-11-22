# RSpecShared, written by Justin Ko <jko170@gmail.com>
# Original at https://github.com/justinko/rspec-shared

module RSpecShared
  module Methods
    def shared(name, &block)
      Thread.current[:rspec] ||= {}
      location = ancestors.last.metadata[:example_group][:location]

      define_method(name) do
        Thread.current[:rspec][location + name.to_s] ||= instance_eval(&block)
      end
    end
  end
end
