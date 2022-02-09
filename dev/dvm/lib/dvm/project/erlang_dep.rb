module DVM
  class ErlangDep < Dep
    include DVM::Tools
    # TODO the path names suck
    attr_reader :ref, :url, :dep_path, :real_path, :parent, :libname, :libpath
    # Needed for now to make tools mixin behave:
    attr_reader :path
    def initialize(name, base_dir, data, parent_inst)
      super(name)
      @deps = nil
      @url = data["url"]
      @ref  = data["ref"]
      @name = name
      @real_path = File.join("/host", name)
      @path = @real_path
      @dep_path = File.join(base_dir, name)
      @parent = parent_inst
      @available = nil
    end

    def loaded?
      puts ("Checking #{dep_path}: #{File.exist?(dep_path)}")
      File.directory?(dep_path)
    end

    def available?
      true
    end

    def load(opts)
      say("To load a dependency, ensure the project is loaded then clone the project into chef-server/external-deps")
      say("See dev/README.md under Erlang Project Dependencies for more information")
    end

    def unload
      load_info
      # restore original-link
      FileUtils.rm(libpath)
      FileUtils.ln_s(dep_path, libpath)
      say(HighLine.color("Restored library link for #{name} to #{libname}.  PLease wait a moment for sync to pick up the change", :green))
      puts ""
    end
  end
end
