require 'yajl'
require 'eventmachine'
require 'amqp'
require 'mq'
require 'opscode/expander/loggable'
require 'opscode/expander/node'
require 'opscode/expander/vnode'
require 'opscode/expander/vnode_table'

module Opscode
  module Expander
    class VNodeSupervisor
      include Loggable

      def initialize
        @vnodes = {}
        @vnode_table = VNodeTable.new
        @local_node  = Node.local_node
        @queue_name, @guid = nil, nil
      end

      def start(vnode_ids)
        start_control_listener
        start_gossip_listener
        start_gossip_publisher
        Array(vnode_ids).each { |vnode_id| spawn_vnode(vnode_id) }
      end

      def vnode_added(vnode)
        log.debug { "vnode #{vnode.vnode_number} registered with supervisor" }
        @vnodes[vnode.vnode_number.to_i] = vnode
      end

      def vnode_removed(vnode)
        log.debug { "vnode #{vnode.vnode_number} unregistered from supervisor" }
        @vnodes.delete(vnode.vnode_number.to_i)
      end

      def vnodes
        @vnodes.keys
      end

      def spawn_vnode(vnode_number)
        VNode.new(vnode_number, self).start
      end

      def start_control_listener
        subscription_confirmed = Proc.new do
          log.info("Listening for control updates on #{control_queue_name}")
        end

        control_queue.subscribe(:ack => true, :confirm => subscription_confirmed) do |header, message|
          log.debug { "Received control message on queue #{control_queue_name}: #{message}"}
          header.ack
        end
      end

      def start_gossip_publisher
        EM.add_periodic_timer(10) do
          current_vnode_status = vnode_status
          current_vnode_status[:action] = 'update'
          gossip_exchange.publish(Yajl::Encoder.encode(current_vnode_status))
        end
      end

      def start_gossip_listener
        subscription_confirmed = Proc.new do
          log.info("Listening for gossip updates on #{gossip_queue_name}")
        end

        gossip_queue.subscribe(:ack => true, :confirm => subscription_confirmed) do |header, message|
          log.debug { "Received gossip message on queue #{gossip_queue_name}: #{message}"}
          @vnode_table.update_table(message)
          log.debug { "Current vnode table: #{@vnode_table.vnodes_by_node.inspect}" }
          header.ack
        end
      end

      def vnode_status
        status = @local_node.to_hash
        status[:vnodes] = vnodes
        status
      end

      def initiate_vnode_recovery(vnode_id)
        raise "TODO"
        #gossip_queue.publish(Yajl::Encoder.encode({:x}))
      end

      def update_global_vnode_table(message)
        Mutex.new.synchronize do
          if message.respond_to?(:key?) && (update = message["vnode_table_update"])
            log.debug { "updating the vnode table for node #{update["guid"]} with nodes [#{update["vnodes"].sort.join(',')}]" }
            vnodes = update["vnodes"]
            @global_vnode_table[update["guid"]] = {:vnodes => vnodes, :vnode_count => vnodes.size}
          else
            log.error { "invalid vnode table update received: #{parsed_message.inspect}" }
          end
        end
      end

      def control_queue
        Mutex.new.synchronize do
          @control_queue ||= begin
            log.debug { "declaring control queue #{control_queue_name}" }
            MQ.queue(control_queue_name, :exclusive => true)
          end
        end
      end

      def gossip_exchange
        Mutex.new.synchronize do
          @gossip_exchange ||= begin
            log.debug { "declaring gossip exchange opscode-platfrom-gossip" }
            MQ.fanout("opscode-platform-gossip")
          end
        end
      end

      def gossip_queue
        Mutex.new.synchronize do
          @gossip_queue ||= begin
            log.debug { "declaring gossip queue #{gossip_queue_name}"}
            q = MQ.queue(gossip_queue_name, :exclusive => true)
            q.bind(gossip_exchange)
            q
          end
        end
      end

      def gossip_queue_name
        @local_node.gossip_queue_name
      end

      def control_queue_name
        @local_node.control_queue_name
      end

    end
  end
end