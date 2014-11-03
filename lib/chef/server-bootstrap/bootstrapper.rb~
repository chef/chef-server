require 'opscode/test/dsl'

module Opscode::Test
  class Bootstrapper
    include Opscode::Test::Configurable
    include Opscode::Test::DSL

    def bootstrap(filename)
      instance_eval(File.read(filename), filename, 1)
    end
  end
end
