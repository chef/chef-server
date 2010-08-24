require 'logger'

module Opscode
  module Expander
    module Loggable
      class Logger
        include Mixlib::Log

        [:debug,:info,:warn,:error, :fatal].each do |level|
          class_eval(<<-LOG_METHOD, __FILE__, __LINE__)
            def #{level}(message, &block)
              @logger.#{level}(message, &block)
            end
          LOG_METHOD
        end
      end

      LOGGER = Logger.new
      LOGGER.init

      def log
        LOGGER
      end

    end
  end
end