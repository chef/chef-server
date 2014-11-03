require 'chef/server-bootstrap/dsl'

module Chef::ServerBootstrap
  class Bootstrapper
    include Chef::ServerBootstrap::Configurable
    include Chef::ServerBootstrap::DSL

    def bootstrap(filename)
      instance_eval(File.read(filename), filename, 1)
    end
  end
end
