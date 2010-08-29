require 'uuidtools'
require 'chef/shell_out'
require 'chef/mixin/shell_out'

module Opscode
  module Expander
    class Node
      extend Chef::Mixin::ShellOut

      def self.from_hash(node_info)
        new(node_info[:guid], node_info[:hostname_f], node_info[:pid])
      end

      def self.local_node
        new(guid, hostname_f, Process.pid)
      end

      def self.guid
        Mutex.new.synchronize do
          return @guid if @guid
          @guid = UUIDTools::UUID.random_create.to_s
        end
      end

      def self.hostname_f
        @hostname ||= shell_out!("hostname -f").stdout.strip
      end

      attr_reader :guid

      attr_reader :hostname_f

      attr_reader :pid

      def initialize(guid, hostname_f, pid)
        @guid, @hostname_f, @pid = guid, hostname_f, pid
      end

      def gossip_queue_name
        @gossip_queue_name ||= "#{identifier}--gossip"
      end

      def control_queue_name
        @control_queue_name ||= "#{identifier}--control"
      end

      def identifier
        "#{hostname_f}--#{pid}--#{guid}"
      end

      def ==(other)
        other.respond_to?(:guid) && other.respond_to?(:hostname_f) && other.respond_to?(:pid) &&
        (other.guid == guid) && (other.hostname_f == hostname_f) && (other.pid == pid)
      end

      def eql?(other)
        (other.class == self.class) && (other.hash == hash)
      end

      def hash
        identifier.hash
      end

      def to_hash
        {:guid => @guid, :hostname_f => @hostname_f, :pid => @pid}
      end

    end
  end
end