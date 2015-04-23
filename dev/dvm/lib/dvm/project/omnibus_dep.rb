module DVM
  class OmnibusDep < Dep
    attr_reader :source_path, :dest_path, :reconfigure_on_load
    def initialize(name, config)
      super(name)
      @source_path = File.join("/host/opscode-omnibus/files", config['source_path'])
      @dest_path = File.join("/opt/opscode/embedded", config['dest_path'])
      @reconfigure_on_load = config['reconfigure_on_load']
      @available = true
    end
    def unload
      unmount(source_path)
      if reconfigure_on_load
        run_command("chef-server-ctl reconfigure", "Reconfiguring chef server to pick up the changes")
      end
    end
    def load
      bind_mount(source_path, dest_path)
      if reconfigure_on_load
        run_command("chef-server-ctl reconfigure", "Reconfiguring chef server to pick up the changes")
      end
    end
    def loaded?
      path_mounted? source_path
    end
  end
end


