class Partybus::CommandRunner

  def run_command(command)
    system(command)
    
    # Returns true, false or nil. In order to return exit status for a
    # command use $?.exitstatus
  end

end
