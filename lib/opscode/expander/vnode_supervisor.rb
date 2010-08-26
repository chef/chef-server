require 'yajl'
require 'uuidtools'
require 'eventmachine'
require 'amqp'
require 'mq'
require 'chef/shell_out'
require 'chef/mixin/shell_out'
require 'opscode/expander/loggable'
require 'opscode/expander/vnode'

module Opscode
  module Expander
    class VNodeSupervisor
      include Loggable
      include Chef::Mixin::ShellOut

      def initialize
        @vnodes = {}
        @global_vnode_table = {}
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
        EM.add_periodic_timer(30) do
          gossip_queue.publish(Yajl::Encoder.encode({:vnode_table_update => vnode_status}))
        end
      end

      def start_gossip_listener
        subscription_confirmed = Proc.new do
          log.info("Listening for gossip updates on #{gossip_queue_name}")
        end

        gossip_queue.subscribe(:ack => true, :confirm => subscription_confirmed) do |header, message|
          log.debug { "Received gossip message on queue #{gossip_queue_name}: #{message}"}
          update_global_vnode_table(message)
          header.ack
        end
      end

      def vnode_status
        {:guid => guid, :hostname => hostname_f, :pid => Process.pid, :vnodes => vnodes}
      end

      def update_global_vnode_table(message)
        Mutex.new.synchronize do
          parsed_message = Yajl::Parser.parse(message)
          if parsed_message.respond_to?(:key?) && (update = parsed_message["vnode_table_update"])
            log.debug { "updating the vnode table for node #{update["guid"]} with nodes [#{update["vnodes"].sort.join(',')}]" }
            @global_vnode_table[update["guid"]] = update["vnodes"]
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

      def gossip_queue
        Mutex.new.synchronize do
          @gossip_queue ||= begin
            log.debug { "declaring gossip queue #{gossip_queue_name}"}
            MQ.queue(gossip_queue_name, :exclusive => true)
          end
        end
      end

      def gossip_queue_name
        return @gossip_queue_name if @gossip_queue_name
        @gossip_queue_name = "#{hostname_f}--#{Process.pid}--#{guid}--gossip"
      end

      def control_queue_name
        return @control_queue_name if @control_queue_name
        @control_queue_name = "#{hostname_f}--#{Process.pid}--#{guid}--control"
      end

      def guid
        Mutex.new.synchronize do
          return @guid if @guid
          @guid = UUIDTools::UUID.random_create.to_s
        end
      end

      def hostname_f
        @hostname ||= shell_out!("hostname -f").stdout.strip
      end

    end
  end
end