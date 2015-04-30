module Pedant
  module Knife
    # Convenience function for grabbing a hash of several important
    # Mixlib::Shellout command configuration parameters.
    def self.command_setting(shellout_command)
      keys = [:cwd, :user, :group, :umask, :timeout, :valid_exit_codes, :environment]
      keys.inject({}) do |hash, attr|
        hash[attr] = shellout_command.send(attr)
        hash
      end
    end

  end
end
