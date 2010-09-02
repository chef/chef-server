require 'bunny'
require 'yajl'
require 'eventmachine'
require 'amqp'
require 'mq'
require 'highline'

require 'opscode/expander/node'

require 'pp'

module Opscode
  module Expander
    class Control

      def self.run(argv)
        new(argv).run
      end

      def self.desc(description)
        @desc = description
      end

      def self.option(*args)
        #TODO
      end

      def self.arg(*args)
        #TODO
      end

      def self.descriptions
        @descriptions ||= []
      end

      def self.method_added(method_name)
        if @desc
          descriptions << [method_name, method_name.to_s.gsub('_', '-'), @desc]
          @desc = nil
        end
      end

      def self.compile
        run_method = "def run; case @argv.first;"
        descriptions.each do |method_name, command_name, desc|
          run_method << "when '#{command_name}';#{method_name};"
        end
        run_method << "else; help; end; end;"
        class_eval(run_method, __FILE__, __LINE__)
      end

      def initialize(argv)
        @argv = argv.dup
      end

      desc "Show this message"
      def help
        puts "Usage: opscode-expanderctl COMMAND"
        puts
        puts "Commands:"
        self.class.descriptions.each do |method_name, command_name, desc|
          puts "  #{command_name}".ljust(15) + desc
        end
      end

      desc "display the aggregate queue backlog"
      def queue_depth
        h = HighLine.new
        total_messages = 0

        amqp_client = Bunny.new(AMQP_CONFIG)
        amqp_client.start

        0.upto(VNODES - 1) do |vnode|
          q = amqp_client.queue("vnode-#{vnode}")
          total_messages += q.status[:message_count]
        end
        puts "  total messages: #{total_messages}"
      ensure
        amqp_client.stop if defined?(amqp_client) && amqp_client
      end

      desc "show the backlog and consumer count for each vnode queue"
      def queue_status
        h = HighLine.new
        queue_status = [h.color("VNode", :bold), h.color("Messages", :bold), h.color("Consumers", :bold)]

        total_messages = 0

        amqp_client = Bunny.new(AMQP_CONFIG)
        amqp_client.start

        0.upto(VNODES - 1) do |vnode|
          q = amqp_client.queue("vnode-#{vnode}")
          status = q.status
          # returns {:message_count => method.message_count, :consumer_count => method.consumer_count}
          queue_status << vnode.to_s << status[:message_count].to_s << status[:consumer_count].to_s
          total_messages += status[:message_count]
        end
        puts "  total messages: #{total_messages}"
        puts
        puts h.list(queue_status, :columns_across, 3)
      ensure
        amqp_client.stop if defined?(amqp_client) && amqp_client
      end

      desc "show the status of the nodes in the cluster"
      def node_status
        status_mutex = Mutex.new
        h = ::HighLine.new
        node_status = [h.color("Host", :bold), h.color("PID", :bold), h.color("GUID", :bold), h.color("Vnodes", :bold)]

        print("Collecting status info from the cluster...")

        AMQP.start(AMQP_CONFIG) do
          node = Expander::Node.local_node
          node.exclusive_control_queue.subscribe do |header, message|
            status = Yajl::Parser.parse(message)
            status_mutex.synchronize do
              node_status << status["hostname_f"]
              node_status << status["pid"].to_s
              node_status << status["guid"]
              # BIG ASSUMPTION HERE that nodes only have contiguous vnode ranges
              # will not be true once vnode recovery is implemented
              node_status << "#{status["vnodes"].min}-#{status["vnodes"].max}"
            end
          end
          node.broadcast_message(Yajl::Encoder.encode(:action => :status, :rsvp => node.exclusive_control_queue_name))
          EM.add_timer(2) { AMQP.stop;EM.stop }
        end

        puts "done"
        puts
        puts h.list(node_status, :columns_across, 4)
        puts
      end


      compile
    end
  end
end