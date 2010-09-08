require 'eventmachine'
require 'amqp'
require 'mq'

require 'opscode/expander/loggable'
require 'opscode/expander/solrizer'

module Opscode
  module Expander
    class VNode
      include Loggable

      attr_reader :vnode_number

      attr_reader :supervise_interval

      def initialize(vnode_number, supervisor, opts={})
        @vnode_number = vnode_number.to_i
        @supervisor   = supervisor
        @queue    = nil
        @stopped  = false
        @supervise_interval = opts[:supervise_interval] || 30
      end

      def start
        @supervisor.vnode_added(self)

        subscription_confirmed = Proc.new do
          abort_on_multiple_subscribe
          supervise_consumer_count
        end

        queue.subscribe(:ack => true, :confirm => subscription_confirmed) do |headers, payload|
          log.debug {"got #{payload} size(#{payload.size} bytes) on queue #{queue_name}"}
          solrizer = Solrizer.new(payload) { headers.ack }
          solrizer.run
        end

      rescue MQ::Error => e
        log.error {"Failed to start subscriber on #{queue_name} #{e.class.name}: #{e.message}"}
      end

      def supervise_consumer_count
        EM.add_periodic_timer(supervise_interval) do
          abort_on_multiple_subscribe
        end
      end

      def abort_on_multiple_subscribe
        queue.status do |message_count, subscriber_count|
          if subscriber_count.to_i > 1
            log.error { "Detected extra consumers (#{subscriber_count} total) on queue #{queue_name}, cancelling subscription" }
            stop
          end
        end
      end

      def stop
        log.debug {"Cancelling subscription on queue #{queue_name.inspect}"}
        queue.unsubscribe if queue.subscribed?
        @supervisor.vnode_removed(self)
        @stopped = true
      end

      def stopped?
        @stopped
      end

      def queue
        @queue ||= begin
          log.debug { "declaring queue #{queue_name}" }
          MQ.queue(queue_name, :passive => false, :durable => true)
        end
      end

      def queue_name
        "vnode-#{@vnode_number}"
      end

      def control_queue_name
        "#{queue_name}-control"
      end

    end
  end
end