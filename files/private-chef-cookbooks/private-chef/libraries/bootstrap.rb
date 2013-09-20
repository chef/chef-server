require 'mixlib/shellout'

class ECBootstrap

  def self.bootstrap_sentinel_file
    "var/opt/opscode/bootstrapped"
  end

  # Use the presence of a sentinel file as an indicator for whether
  # the server has already had initial bootstrapping performed.
  def self.has_been_bootstrapped?
    File.exists?("/var/opt/opscode/bootstrapped")
  end
end
