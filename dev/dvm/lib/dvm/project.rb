require "mixlib/shellout"

module DVM
  class Project
    attr_reader :name, :project, :config, :service, :project_dir

    # TODO check required fields in config
    def initialize(project_name, config)
      @name = project_name
      @config = config
      @project = config['projects'][project_name]
      @service = @project['service']
      @project_dir = "/host/#{name}"
    end
    def self.safe_run_command(command, desc = nil, opts = {})
      say desc if desc
      cmd = Mixlib::ShellOut.new(command, opts)
      cmd.run_command
      cmd.error!
    end
    def self.bind_mount(from, to)
      safe_run_command "mount -o bind #{from} #{to}"
    end
    def self.unmount(from)
      safe_run_command "umount #{from}"
    end

    def self.path_mounted?(path)
      mounts = `mount`
      mounts.split("\n").each do |mount|
        if mount.include? path
          return true
        end
      end
      return false
    end

    def start(args,odetach)
      raise DVM::DVMArgumentError, "Start not supported for #{name}"
    end

    def run(args)
      raise DVM::DVMArgumentError, "Run not supported for #{name}"
    end

    def database
      raise DVM::DVMArgumentError, "No database configured for #{name}" unless @project['database']
      @project['database']
    end

    def deps
      parse_deps
      @deps
    end

    def load(build)

    #  parse_deps
    end

    def loaded?
      false
    end
    def etop
      raise DVM::DVMArgumentError, "This application does not support etop."
    end

    def load_dep(name, build)
      # TODO separate impl so we can ensure parse_deps and "loaded?"
      raise DVM::DVMArgumentError, "This application does not support loading dependencies."
    end

    def unload
      raise "You should implement this now."
    end

    def update(args)
      raise DVM::DVMArgumentError, "This project does not support dynamic updates."
    end

    def console
      raise DVM::DVMArgumentError, "This project does not support a console."
    end

    def ensure_dep(name)
      raise DVM::DVMArgumentError, "This project does not have the dep #{name}" unless deps.has_key? name
    end

    def parse_deps
      @deps = {}
    end
    def unload_dep(name)
      raise DVM::DVMArgumentError, "Not yet supported"
    end
  end


  class ErlangProject < Project
    class ProjectDep
      attr_reader :name, :ref, :url, :path
      def initialize(name, base_dir, data)
        @url = data["url"]
        @ref  = data["ref"]
        @name = name
        @path = "#{base_dir}/#{name}"
      end
      def loaded?
        File.symlink? path
      end
      def load
      end
      def unload
        raise DVMArgumentError,  "#{name} is not loaded" unless loaded?
        stop() if is_running?
        Project.unmount(@project_dir)
        # werlang project: for dep unloading nuke the dep and get-deps, compile seems fastest
        # unless we want to do the preserve dance.
      end
    end
    attr_reader :rebar_config_path, :project_dir, :relpath
    def initialize(project_name, config)
      super
      # TODO use .lock if available, else use .config
      @rebar_config_path = "#{project_dir}/rebar.config.lock"
      reldir = service['rel-type'] == 'relx' ? "_rel" : "rel"
      @relpath = "#{@project_dir}/#{reldir}/#{name}"
      @service_dir = "/var/opt/opscode/embedded/service/#{service['name']}"
    end

    def parse_deps
      @deps = {}
      # TODO when project is unloaded, parse.es load might have parse.es in ../..
      path = File.expand_path("../../../parse.es", __FILE__)
      eval(`#{path} #{@rebar_config_path}`).each do |name, data|
        @deps[name] = ProjectDep.new(name, "#{project_dir}/deps", data)
      end
    end

    def is_running?
      `ps waux | grep "host/#{name}.*/run_erl"`.split("\n").length > 2
    end

    def start(args, background)
      raise DVMArgumentError, "#{name} appears to be already running. Try 'dvm console oc_erchef', or 'dvm stop oc_erchef'" if is_running?
      if background
        Project.safe_run_command("bin/#{name} start -env DEVVM=1", "Starting #{name}", cwd: relpath, env: { "DEVVM" => "1" } )
      else
        exec "cd #{relpath} && bin/#{name} console"
      end
    end
    def stop()
      raise DVMArgumentError, <<-EOM unless is_running?
"#{name} does not seem to be running from a loaded instance.
If you want to stop the global instance, use chef-server-ctl stop #{service['name']}'"
EOM
      Project.safe_run_command("bin/#{name} stop", "Stoopping #{name}", :cwd => relpath)
    end
    def console
      raise "#{name} does not seem to be running from a loaded instance. Use `dvm start #{name}` to start it" unless is_running?
      exec "#{erl_command} -remsh #{service["node"]}"
    end
    def loaded?
      return @loaded unless @loaded.nil?
      libpath =  File.join(@relpath, "lib")
      # If a devrel has been performed on this box, almost everything in the
      # lib directory will actually be a symlink to a valid location on this box.
      # Filter out everything that fits this criteria - if any files remain
      # then a proper devrel has not been performed.
      #
      begin
        files = Dir.entries(libpath)
        if files.length < 4
          @loaded = false
          @loaded
        end
      rescue
        @loaded = false
        return @loaded
      end
      files.reject! do |fname|
        fullpath = File.join(libpath, fname)
        # TODO clean case!

        # special cases, not symlinks:
        (fname == "patches" or fname == "." or fname == "..") or
          # all else must be both a symlink and resolve to valid location
          (File.symlink?(fullpath) and File.file?(fullpath))

      end
      @loaded = files.length == 0
      @loaded

    end

    def load_dep(name, build)
      # TODO wrap this in parent class
      raise DVM::DVMArgumentError, "Load the project before loading deps." unless loaded?
      ensure_dep

    end

    def load(no_build = false)
      raise DVM::DVMArgumentError, "Project not available. Clone it onto your host machine." unless File.directory?(@project_dir)
      raise DVM::DVMArgumentError, "Project already loaded" if loaded?
      do_build unless no_build
      Project.safe_run_command("chef-server-ctl stop #{service['name']}", "Stopping #{service['name']}", cwd: project_dir)

      say("Setting up symlinks")
      FileUtils.rm_rf(["#{relpath}/log", "#{relpath}/sys.config"])
      FileUtils.ln_s("/var/opt/opscode/#{service["name"]}/sys.config", "#{relpath}/sys.config")
      FileUtils.ln_s("/var/log/opscode/#{service["name"]}", "#{relpath}/log")

      say(HighLine.color("Success! Your project is loaded.", :green))
      say("Start it now:")
      say(HighLine.color("    dvm start #{name} [--background]", :green))
    end

    def do_build
      say("This should take just a couple of minutes")
      say("Cleaning deps...")
      FileUtils.rm_rf(["#{project_dir}/deps"])
      Project.safe_run_command("make clean relclean", "Cleaning up everything else",     cwd: project_dir)
      Project.safe_run_command("rebar get-deps -C rebar.config.lock",       "Getting all deps, please hold.", cwd: project_dir)
      Project.safe_run_command("make -j 8 devrel",    "Building, this will take a few.", cwd: project_dir, env: { "USE_SYSTEM_GECODE" => "1"})
    end

    def enable_cover(modules = nil)
      # [file:wildcard("/host/oc_erchef/apps/*/ebin")
      # compile_beam_dir("/host/oc_erchef/apps/
    end
    def dump_cover(modules = nil)

    end
    def update(args)
      # No running check in case we're trying to attach to the global instance
      node = service['node']
      if args == "pause"
        pause_msg = "Pausing sync on #{node}. This will take effect after any running scans have been completed. \nUse 'dvm update oc_erchef resume' to re-enable automatic updates."
        Project.safe_run_command("#{erl_command} -eval \"rpc:call('#{node}', sync, pause, []).\" -s erlang halt", pause_msg)
      elsif args == "resume"
        Project.safe_run_command("#{erl_command} -eval \"rpc:call('#{node}', sync, go, []).\" -s erlang halt", "Resuming sync on #{node}")
      end
    end

    def etop
      exec "#{erl_command} -s etop -s erlang halt -output text -sort reductions -lines 25 -node #{service['node']}"
    end

    def erl_command
      "erl -hidden -name dvm@127.0.0.1 -setcookie #{service["cookie"]}"
    end

  end


  class RubyProject < Project
    def initialize(project_name, config)
      super
    end
    def load( no_build)
      if @project.has_key?('load')
        # TODO this seems like a sane default for load behavior in Project, have it
        # call subclass do_load if there is no such key. Will also let us quickly get new projects
        # going.
        @project['load'].each do |c|
          Project.safe_run_command(c, c, cwd: @project_dir)
        end
      else
        Project.safe_run_command("rm -rf .bundle/config && bundle install --path /opt/opscode/embedded/service/gem --no-binstubs", "Installing in-place...", cwd: @project_dir)
      end
    end
    def unload
      say "Unmounting #{name}"
      Project.unmount(@project_dir)
    end
    def loaded?
      false
    end
    def run(args)
      exec "cd #{@project_dir} && #{@project['run']} #{args.join(" ")}"
    end
  end



  class OmnibusProject < Project
    class OmnibusDep
      attr_reader :name, :source_path, :dest_path, :reconfigure_on_load
      def initialize(name, config)
        @name = name
        @source_path = File.join("/host/opscode-omnibus/files", config['source_path'])
        @dest_path = File.join("/opt/opscode/embedded", config['dest_path'])
        @reconfigure_on_load = config['reconfigure_on_load']
      end
      def unload
        Project.unmount(source_path)
        if reconfigure_on_load
          Project.safe_run_command("chef-server-ctl reconfigure", "Reconfiguring chef server to pick up the changes")
        end
      end
      def load
        Project.bind_mount(source_path, dest_path)
        if reconfigure_on_load
          Project.safe_run_command("chef-server-ctl reconfigure", "Reconfiguring chef server to pick up the changes")
        end
      end
      def loaded?
        Project.path_mounted? source_path
      end
    end
    def initialize(project_name, config)
      super
    end
    def unload_dep(subname)
      parse_deps
      raise DVMArgumentError, "Could not find sub-project #{subname}" unless  @deps.has_key? subname
      dep = @deps[subname]
      raise DVMArgumentError, "#{subname} is not loaded." unless dep.loaded?
      dep.unload
    end
    def load_dep(subname, ignored)
      parse_deps
      raise DVMArgumentError, "Could not find sub-project #{subname}" unless  @deps.has_key? subname
      dep = @deps[subname]
      raise DVMArgumentError, "#{subname} is already loaded." if dep.loaded?
      dep.load
    end
    def parse_deps()
      #  for us, deps are more subprojects.
      if @deps.nil?
        @deps = {}
        @project['components'].each do |sub, c|
          @deps[sub] = OmnibusDep.new(sub, c)
        end
      end
    end
    def loaded?
      File.directory? @project_dir
    end
  end
end
