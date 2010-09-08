require 'opscode/expander/version'

module Opscode
  module Expander
    class ClusterSupervisor


      def initialize(cluster_size)
        Expander.init_config(ARGV)

        log.info("Opscode Expander #{VERSION} starting up.")

        
      end

      def run
        start_workers
        maintain_workers
      end

      def start_workers
      end

      def maintain_workers
        
      end

    end
  end
end