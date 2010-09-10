require 'mixlib/log'

module Opscode
  module Expander
    module Loggable
      class Logger
        include Mixlib::Log

        def init(*args)
          @logger = nil
          super
        end

        [:debug,:info,:warn,:error, :fatal].each do |level|
          class_eval(<<-LOG_METHOD, __FILE__, __LINE__)
            def #{level}(message=nil, &block)
              @logger.#{level}(message, &block)
            end
          LOG_METHOD
        end
      end

      # TODO: it's admittedly janky to set up the default logging this way.
      STDOUT.sync = true
      LOGGER = Logger.new
      LOGGER.init
      LOGGER.level = :debug

      def log
        LOGGER
      end

    end
  end
end

