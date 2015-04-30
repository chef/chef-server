require 'open3'

module Opscode
  module Expander


    def self.git_sha
      Open3.popen3("git rev-parse HEAD") {|stdin, stdout, stderr| stdout.read }.strip
    rescue Errno::ENOENT
      ""
    end

    rev = git_sha
    rev = "release" if rev.empty?

    VERSION = "0.1.0 (#{rev})"

  end
end
