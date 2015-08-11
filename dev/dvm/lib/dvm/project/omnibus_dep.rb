module DVM
  class OmnibusDep < Dep
    attr_reader :source_path, :dest_path, :reconfigure_on_load, :bundler
    # Needed for now to make tools mixin behave:
    attr_reader :path
    def initialize(base_dir, name, config)
      super(name)
      @source_path = File.join("#{base_dir}/files", config['source_path'])
      @dest_path = config['dest_path']
      @path = @source_path
      @reconfigure_on_load = config['reconfigure_on_load']
      @available = true
      @bundler = config['bundler']
    end
    def unload
      unmount(source_path)
      if reconfigure_on_load
        run_command("chef-server-ctl reconfigure", "Reconfiguring chef server to pick up the changes")
      end
    end
    def load(opts)
      if bundler
        run_command("rm -rf .bundle/config ;  bundle install --path /opt/opscode/embedded/service/gem --no-binstubs", "Installing in-place...", cwd: @dest_path)
      end
      bind_mount(source_path, dest_path)
      if reconfigure_on_load and not opts[:no_build]
        run_command("chef-server-ctl reconfigure", "Reconfiguring chef server to pick up the changes")
      end

    end
    def loaded?
      path_mounted? source_path
    end
  end
end


