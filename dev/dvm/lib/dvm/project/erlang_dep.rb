module DVM
  class ErlangDep < Dep
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
      load_info
      @link_target == @real_path
    end

    def available?
      load_info
      @available
    end
    def load_info
      return unless @available == nil
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
      @available = true
      @libpath = matches[0]
      @libname = File.basename(matches[0])
      @link_target = File.symlink?(libpath) ? File.readlink(libpath) : libpath
    end


    def load(opts)
      load_info
      # Again, muich of this can be offloaded to a base class that hooks into child class via callbacks.
      if !project_dir_exists_on_host?(name)
        # Some things to consider:
        # do we want to match the revision/branch from rebar?
        # do we want to auto-create a new branch from it if we did the clone ourselves or detect master or
        if opts[:skip_checkout]
          raise DVMArgumentError "The project directory for #{name} can't be found, and you told me not to check it out."
        end
        # Ensure we're starting with the same code base that we had in the dependency to avoid
        # hot-loading headaches.
        clone(name, url)
      end
      # TODO if project is not running, do a normal build of the project - or shoudl we anyway so that
      #      our beam files persist?

      checkout(name, ref) unless opts[:skip_checkout]
      # INstead of linking it into the dep directory, replace the library path in the project installation
      # That way we don't have to overlay or preserve the project dep in it's original state - we'll
      # just restore the link on unload.
      FileUtils.rm(libpath)
      FileUtils.ln_s(real_path, libpath)
      say(HighLine.color("The dependency has been loaded, please wait a moment for sync to pick up the change.", :green))
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
