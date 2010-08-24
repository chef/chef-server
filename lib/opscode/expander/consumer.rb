require 'opscode/expander/version'
require 'opscode/expander/loggable'
module Opscode
  module Expander
    class Consumer
      extend  Loggable
      include Loggable

      def self.run
        log.info("Opscode Expander #{VERSION} starting up.")
        log.debug("Starting Index Queue Consumer")

        AMQP.start do

        end
      end

      class << self
        alias :start :run
      end

      def call_action_for_message(message)
        amqp_payload  = JSON.parse(message[:payload], :create_additions => false, :max_nesting => false)
        action        = amqp_payload["action"].to_sym
        app_payload   = amqp_payload["payload"]
        assert_method_whitelisted(action)
        send(action, app_payload)
      end

      def assert_method_whitelisted(method_name)
        unless WHITELIST.include?(method_name)
          raise ArgumentError, "non-whitelisted method #{method_name} called via index queue (only #{WHITELIST.join(', ')} are allowed)"
        end
      end

      def add(payload)
        index = Chef::Solr::Index.new
        Chef::Log.debug("Dequeued item for indexing: #{payload.inspect}")

        begin
          # older producers will send the raw item, and we no longer inflate it
          # to an object.
          pitem = payload["item"].to_hash
          pitem.delete("json_class")
          response = generate_response { index.add(payload["id"], payload["database"], payload["type"], pitem) }                  
        rescue NoMethodError
          response = generate_response() { raise ArgumentError, "Payload item does not respond to :keys or :to_hash, cannot index!" }
        end

        msg = "Indexing #{payload["type"]} #{payload["id"]} from #{payload["database"]} status #{status_message(response)}}"
        Chef::Log.info(msg)
        response 
      end

      def delete(payload)
        response = generate_response { Chef::Solr::Index.new.delete(payload["id"]) }
        Chef::Log.info("Removed #{payload["id"]} from the index")
        response
      end

      private

      def generate_response(&block)
        response = {}
        begin
          block.call
        rescue => e
          response[:status] = :error
          response[:error] = e
        else
          response[:status] = :ok
        end
        response
      end

      def status_message(response)
        msg = response[:status].to_s
        msg << ' ' + response[:error].to_s if response[:status] == :error
        msg
      end

    end
  end
end