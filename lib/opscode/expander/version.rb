module Opscode
  module Expander
    rev = `git rev-parse HEAD`.strip
    VERSION = "0.1.0"
    VERSION <<  "(r#{rev})" unless rev.empty?
  end
end