require "chef/shell_out"
require 'chef/mixin/shell_out'

module Opscode
  module Expander
    extend Chef::Mixin::ShellOut

    rev = shell_out('git rev-parse HEAD').stdout.strip
    VERSION = "0.1.0"
    VERSION <<  " (#{rev})" unless rev.empty?

  end
end