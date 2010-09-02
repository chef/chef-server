require 'optparse'
require 'singleton'

require 'opscode/expander/version'

module Opscode
  module Expander
    class Config

      class InvalidConfiguration < StandardError
      end

      include Singleton

      attr_accessor :index

      attr_accessor :node_count

      def initialize
        reset!
      end

      def reset!(stdout=nil)
        @index, @node_count = nil, nil
        @stdout = stdout || STDOUT
      end

      def fail_if_invalid
        validate!
      rescue InvalidConfiguration => e
        @stdout.puts("Invalid configuration: #{e.message}")
        exit(1)
      end
      

      def validate!
        unless @node_count.kind_of?(Integer)
          raise InvalidConfiguration, "You need to specify the total number of nodes in the cluster"
        end
        unless @index.kind_of?(Integer)
          raise InvalidConfiguration, "You need to specify this node's index (position in the ring)"
        end
        unless @index <= @node_count
          raise InvalidConfiguration, "the index can't be larger than the node count dawg"
        end
      end

      def vnode_numbers
        vnodes_per_node = VNODES / node_count
        lower_bound = (index - 1) * vnodes_per_node
        upper_bound = lower_bound  + vnodes_per_node
        upper_bound += VNODES % vnodes_per_node if index == node_count
        (lower_bound...upper_bound).to_a
      end

    end

    module CLI
      @config = Config.instance

      @option_parser = OptionParser.new do |o|
        o.banner = "Usage: opscode-expander [options]"

        o.on('-i', '--index INDEX', 'the slot this node will occupy in the ring') do |i|
          @config.index = i.to_i
        end

        o.on('-n', '--node-count NUMBER', 'the number of nodes in the ring') do |n|
          @config.node_count = n.to_i
        end

        o.on_tail('-h', '--help', 'show this message') do
          puts "opscode-expander #{VERSION}"
          puts ''
          puts o
          exit 1
        end

        o.on_tail('-v', '--version', 'show the version and exit') do
          puts "opscode-expander #{VERSION}"
          exit 0
        end

      end

      def self.parse_options(argv)
        @option_parser.parse!(argv.dup)
      end

      def self.config
        @config
      end

    end
  end
end