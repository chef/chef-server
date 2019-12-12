require 'mixlib/shellout'
module Du
  # Calculate the disk space used by the given path. Requires that
  # `du` is in our PATH.
  #
  # @param path [String] Path to a directory on disk
  # @return [Integer] KB used by directory on disk
  #
  def self.du(path)
    # TODO(ssd) 2017-08-18: Do we need to worry about sparse files
    # here? If so, can we expect the --apparent-size flag to exist on
    # all of our platforms.
    command = Mixlib::ShellOut.new("du -sk #{path}")
    command.run_command
    if command.status.success?
      command.stdout.split("\t").first.to_i
    else
      Chef::Log.error("du -sk #{path} failed with exit status: #{command.exitstatus}")
      Chef::Log.error("du stderr: #{command.stderr}")
      raise 'du failed'
    end
  rescue Errno::ENOENT
    raise 'The du utility is not available. Unable to check disk usage'
  end
end
