require 'yajl'
require 'opscode/expander/node'


module Opscode
  module Expander
    class VNodeTable

      class InvalidVNodeTableUpdate < ArgumentError; end

      attr_reader :vnodes_by_node

      def initialize
        @vnodes_by_node = {}
      end

      def nodes
        @vnodes_by_node.keys
      end

      def update_table(node_status_update)
        node_info = json_parser.parse(node_status_update)
        case node_info[:action]
        when "add", "update"
          update_node(node_info)
        when "remove"
          remove_node(node_info)
        else
          raise InvalidVNodeTableUpdate, "no action or action not acceptable: #{node_info.inspect}"
        end
      end

      def update_node(node_info)
        Mutex.new.synchronize do
          @vnodes_by_node[Node.from_hash(node_info)] = node_info[:vnodes]
        end
      end

      def remove_node(node_info)
        Mutex.new.synchronize do
          @vnodes_by_node.delete(Node.from_hash(node_info))
        end
      end

      def least_loaded_node
        if @vnodes_by_node.empty?
          nil
        else
          Array(@vnodes_by_node).sort { |a,b| a[1].size <=> b[1].size }.first[0]
        end
      end

      def json_parser
        Yajl::Parser.new(:symbolize_keys => true)
      end

    end
  end
end