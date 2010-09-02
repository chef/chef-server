require 'eventmachine'
require 'em-http-request'
require 'mq'
require 'amqp'

require 'opscode/expander/version'
require 'opscode/expander/loggable'

require 'opscode/expander/vnode_supervisor'

require 'opscode/expander/config'

module Opscode
  module Expander
    class Consumer
      extend  Loggable
      include Loggable

      def self.start
        @vnode_supervisor = VNodeSupervisor.new
        Kernel.trap(:INT)  { stop(:INT) }
        Kernel.trap(:TERM) { stop(:INT) }

        CLI.parse_options(ARGV)

        log.info("Opscode Expander #{VERSION} starting up.")

        AMQP.start(:user => "guest", :pass => "guest", :vhost => "/testing") do
          log.debug { "Setting prefetch count to 5"}
          MQ.prefetch(5)

          vnodes = Config.instance.vnode_numbers
          log.info("Starting Consumers for vnodes #{vnodes.first}-#{vnodes.last}")
          @vnode_supervisor.start(vnodes)
        end
        
      end

      def self.stop(signal)
        log.info { "Stopping opscode-expander on signal (#{signal})" }
        @vnode_supervisor.stop
        EM.add_timer(3) do
          AMQP.stop
          EM.stop
        end
      end

    end
  end
end