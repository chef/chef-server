module DVM
  class ErlangDep < Dep
    # TODO the path names suck
    attr_reader :ref, :url, :dep_path, :real_path, :parent, :libname, :libpath, :checkouts_path
    # Needed for now to make tools mixin behave:
    attr_reader :path
    def initialize(name, base_dir, data, parent_inst)
      super(name)
      @deps = nil
      @url = data["url"]
      @ref  = data["ref"]
      @name = name
      @parent = parent_inst
      @real_path = real_path
      @path = @real_path
      @dep_path = File.join(base_dir, name)
      @checkouts_path =  File.join(parent.project_dir, "_checkouts", name)
      @available = nil
    end

    def loaded?
      load_info
      File.exists?(@checkouts_path)
    end

    def available?
      load_info
      @available
    end
    def load_info
      return unless @available == nil
      @available = true
      @lib_path = lib_path
      @libname = File.basename(@lib_path)
    end

    def lib_path
      # TODO handle multi matches, also what if we are truly a new version?
      # TODO smarter search, using project rel file and version? But not all have them...
      matches = Dir.glob(File.join(parent.libpath, "#{name}-*"))
      if matches.length == 0
        # Some deps are present at build-time, but not specified in app.src as an application,
        # such as rebar_lock_deps_plugin.
        # TODO We may be able to handle this by just linking it in anyway, need to experiment with it -
        # the potential case for it is if a project has a build-time dep that it needs for sync to
        # compile and load it successfully.
        @available = false
        return
      end
      matches[0]
    end

    def real_path
      if !external_deps_dir_exists_on_host?(name)
        say(HighLine.color("#{host_external_deps_dir(name)} not found, need to copy or link the dependency there in the host system.", :red))
        nil
      else
        host_external_deps_dir(name)
      end
    end

    #
    # We used to checkout/clone the dir in place, now we link to external-deps, which is copied over from the host by sync.
    #
    def load(opts)
      load_info
      if !loaded?
        #
        # Rebar3 provides the _checkouts dir, which overrides deps if present. To load we link to external deps
        #
        FileUtils.ln_s(@real_path, @checkouts_path)
        say(HighLine.color("Added link to #{@real_path} in #{@checkouts_path}, rebar3 will use it instead of #{@lib_path}. Please wait a moment for rebuild to pick up the change.", :green))
      else
        say(HighLine.color("Already loaded #{name} with link to #{@real_path} in #{checkouts_path}.", :green))
      end
    end

    def unload
      load_info
      if loaded?
        # restore original-link
        FileUtils.rm(checkouts_path)
        say(HighLine.color("Removed link for #{name} in #{checkouts_path}, rebar3 will use the code in #{@lib_path}. Please wait a moment for rebuild to pick up the change", :green))
        puts ""
      else
         say(HighLine.color("#{name} not loaded in #{checkouts_path}", :red))
        puts ""
      end
    end
  end
end
