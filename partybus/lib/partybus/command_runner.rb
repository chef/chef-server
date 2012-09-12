class Partybus::CommandRunner

  def run_command(command)
    return_val = system(command)

    unless return_val
      raise "Partybus failed to execute command '#{command}' exitstatus: #{$?.exitstatus}"
    end
  end

end
