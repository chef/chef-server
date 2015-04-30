class Partybus::CommandRunner

  def run_command(command, options={})

    cwd     = options[:cwd]     || Dir.pwd
    env     = options[:env]     || ENV
    returns = options[:returns] || [0]

    Dir.chdir(cwd) do
      Bundler.clean_system(env, command)
    end

    exit_code = $?.exitstatus

    unless returns.include?(exit_code)
      raise "Partybus failed to execute command '#{command}' exitstatus: #{exit_code}"
    end
  end

end
