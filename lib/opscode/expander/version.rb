require 'open3'

module Opscode
  module Expander

    rev = Open3.popen3("git rev-parse HEAD") {|stdin, stdout, stderr| stdout.read }.strip
    VERSION = "0.1.0"
    VERSION <<  " (#{rev})" unless rev.empty?

  end
end