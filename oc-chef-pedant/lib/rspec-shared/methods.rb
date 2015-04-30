# RSpecShared, written by Justin Ko <jko170@gmail.com>
# Original at https://github.com/justinko/rspec-shared

module RSpecShared
  module Methods
    def shared(name, &block)
      # Set the key to be scoped to the parent example group
      # This potentially allows overriding shared() values, but I am not
      # sure of the utility of that.
      parent = ancestors.last
      location = if parent.respond_to?(:metadata)
                   parent.metadata[:example_group][:location]
                 else
                   'rspec_global_'
                 end
      key = location + name.to_s

      # This might have some thread-safety issues
      # The original implementation puts this into Thread[:rspec]. We implement
      # a simple Stash to have better control over it. @jkeiser had noticed
      # intermittent errors in the original implementation.
      define_method(name) do
        Stash[key] ||= instance_eval(&block)
      end
    end
  end

  class Stash
    @@_stash = {}

    def self.[](key)
      mutex.synchronize do
        @@_stash[key]
      end
    end

    # This is implemented as immutable
    def self.[]=(key, value)
      mutex.synchronize do
        @@_stash[key] = value unless @@_stash[key]
      end
    end

    private
    def self.mutex
      @@_mutex ||= Mutex.new
    end
  end
end
