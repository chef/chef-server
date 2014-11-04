require 'opscode/server-bootstrap/dsl'

module Opscode::ServerBootstrap
  class Bootstrapper
    include Opscode::ServerBootstrap::Configurable
    include Opscode::ServerBootstrap::DSL

    def bootstrap(filename)
      instance_eval(File.read(filename), filename, 1)
    end
  end
end
