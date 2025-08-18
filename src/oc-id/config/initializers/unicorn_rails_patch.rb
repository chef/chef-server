# Monkey-patch Rack::Handler.register for unicorn-rails compatibility
if defined?(Rack) && defined?(Rack::Handler) && !Rack::Handler.respond_to?(:register)
  module Rack
    module Handler
      def self.register(server, handler)
        const_set(server.capitalize, handler)
      end
    end
  end
end
