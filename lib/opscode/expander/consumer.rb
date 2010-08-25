require 'eventmachine'
require 'em-http-request'
require 'mq'
require 'amqp'

require 'chef/solr/index'

require 'opscode/expander/version'
require 'opscode/expander/loggable'

require 'opscode/expander/vnode'

module Opscode
  module Expander
    class Consumer
      extend  Loggable
      include Loggable

      def self.start
        log.info("Opscode Expander #{VERSION} starting up.")
        log.debug("Starting Index Queue Consumer")

        AMQP.start(:user => "guest", :pass => "guest", :vhost => "/testing") do
          log.debug { "Setting prefetch count to 1"}
          MQ.prefetch(1)

          log.debug { "declaring topic exchange #{TOPIC_EXCHANGE.inspect}" }
          MQ.topic(TOPIC_EXCHANGE, :durable => true)

          0.upto(VNODES) do |vnode_number|
            VNode.new(vnode_number).start
          end
        end
        
      end

    end
  end
end