require 'time'

class Partybus::MigrationUtil

  # Move any mover log files from a previous run, if they exist.
  # The log message parser requires a "clean slate".
  def clean_mover_logs
    runner = Partybus::CommandRunner.new

    current_time = Time.now.utc.iso8601
    mover_log_file_glob = "/var/log/opscode/opscode-chef-mover/console.log*"
    parsed_log_output = "/var/log/opscode/opscode-chef-mover/parsed_console.log"
    runner.run_command("mkdir /var/log/opscode/opscode-chef-mover/#{current_time}", {})
    begin
      runner.run_command("mv #{mover_log_file_glob} /var/log/opscode/opscode-chef-mover/#{current_time}", {})
    rescue
      log "No files found at #{mover_log_file_glob}. Moving on..."
    end

    begin
      runner.run_command("mv #{parsed_log_output} /var/log/opscode/opscode-chef-mover/#{current_time}", {})
    rescue
      log "#{parsed_log_output} not found. Moving on..."
    end
  end

end
