require 'opscode/expander/loggable'
require 'opscode/expander/version'
require 'opscode/expander/vnode_supervisor'

module Opscode
  module Expander
    class ClusterSupervisor
      include Loggable

      def initialize
        @workers = {}
        @running = true
        @kill    = :TERM
      end

      def start
        trap(:INT)  { stop(:INT) }
        trap(:TERM) { stop(:TERM)}
        Expander.init_config(ARGV)

        log.info("Opscode Expander #{VERSION} starting cluster with #{Expander.config.node_count} nodes")
        
        start_workers
        maintain_workers
      end

      def start_workers
        Expander.config.node_count.times do |i|
          start_worker(i + 1)
        end
      end

      def start_worker(index)
        log.info { "Starting cluster worker #{index}" }
        worker_params = {:index => index}
        child_pid = fork do
          Expander.config.index = index
          VNodeSupervisor.start_cluster_worker
        end
        @workers[child_pid] = worker_params
      end

      def stop(signal)
        log.info { "Stopping cluster on signal (#{signal})" }
        @running = false
        @kill    = signal
      end

      def maintain_workers
        while @running
          sleep 1
          workers_to_replace = {}
          @workers.each do |process_id, worker_params|
            if result = Process.waitpid2(process_id, Process::WNOHANG)
              log.error { "worker #{worker_params[:index]} (PID: #{process_id}) died with status #{result[1].exitstatus || '(no status)'}"}
              workers_to_replace[process_id] = worker_params
            end
          end
          workers_to_replace.each do |dead_pid, worker_params|
            @workers.delete(dead_pid)
            start_worker(worker_params[:index])
          end
        end

        @workers.each do |pid, worker_params|
          log.info { "Stopping worker #{worker_params[:index]} (PID: #{pid})"}
          Process.kill(@kill, pid)
        end
        @workers.each do |pid, worker_params|
          Process.waitpid2(pid)
        end

      end

    end
  end
end