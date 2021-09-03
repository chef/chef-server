require "mixlib/shellout" unless defined?(Mixlib::ShellOut)
module Du
  # Calculate the disk space used by the given path. Requires that
  # `du` is in our PATH.
  #
  # @param path [String] Path to a directory on disk
  # @return [Integer] KB used by directory on disk
  #
  def self.du(path)
    command = Mixlib::ShellOut.new("du -sk #{path}")
    command.run_command
    if command.status.success?
      command.stdout.split("\t").first.to_i
    else
      puts "du -sk #{path} failed with exit status: #{command.exitstatus}"
      puts "du stderr: #{command.stderr}"
      raise "DuFailedException", command.stderr
    end
  rescue Errno::ENOENT
    raise "DuFailedException", command.stderr
  end
end
