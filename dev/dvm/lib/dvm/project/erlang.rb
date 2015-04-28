require_relative "erlang_dep"

module DVM
  class ErlangProject < Project
    attr_reader :rebar_config_path, :project_dir, :relpath, :libpath
    def initialize(project_name, config)
      super
      # TODO use .lock if available, else use .config
      @rebar_config_path = "#{project_dir}/rebar.config.lock"
      reldir = service['rel-type'] == 'relx' ? "_rel" : "rel"
      @relpath = "#{@project_dir}/#{reldir}/#{name}"
      @service_dir = "/var/opt/opscode/embedded/service/#{service['name']}"
      @libpath =  File.join(@relpath, "lib")
    end

    def parse_deps
      return @deps if @deps
      @deps = {}
      path = File.expand_path("../../../../parse.es", __FILE__)
      # For some reason vim tag browser doesn't like it when
      # eval(`...`) is used.
      result = run_command("#{path} #{@rebar_config_path}")
      eval(result.stdout).each do |name, data|
        @deps[name] = ErlangDep.new(name, File.join(project_dir, "deps") , data, self)
      end
    end

    def is_running?
      `ps waux | grep "host/#{name}.*/run_erl"`.split("\n").length > 2 ||
      `ps waux | grep "host/#{name}.*/beam.smp"`.split("\n").length > 2
    end

    def start(args, background)
      raise DVMArgumentError, "#{name} appears to be already running. Try 'dvm console oc_erchef', or 'dvm stop oc_erchef'" if is_running?
      if background
        run_command("bin/#{name} start -env DEVVM=1", "Starting #{name}", cwd: relpath, env: { "DEVVM" => "1" } )
      else
        exec "cd #{relpath} && bin/#{name} console"
      end
    end

    def stop()
      raise DVMArgumentError, <<-EOM unless is_running?
"#{name} does not seem to be running from a loaded instance.
If you want to stop the global instance, use chef-server-ctl stop #{service['name']}'"
EOM
      run_command("bin/#{name} stop", "Stoopping #{name}", :cwd => relpath)
    end

    def console
      raise "#{name} does not seem to be running from a loaded instance. Use `dvm start #{name}` to start it" unless is_running?
      exec "#{erl_command} -remsh #{service["node"]}"
    end

    def loaded?
      return @loaded unless @loaded.nil?
      # If a devrel has been performed on this box, almost everything in the
      # lib directory will actually be a symlink to a valid location on this box.
      # Filter out everything that fits this criteria - if any files remain
      # then a proper devrel has not been performed.
      #
      # Addendum:
      # looks like some concrete projects don't properly link system libraries
      # into the lib dir, so we'll to verify if one of these is true:
      # - verify as we are here, which assumes a proprly linked lib dir
      # - verify that everything in 'deps' is linked to in lib dir,
      #   which matches the naive approach taken by concrete projects
      #   TODO - maybe just another config item sthat specifies expected concrete behavior?

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
       # special cases, not symlinks:
        (fname == "patches" or fname == "." or fname == "..") or
          # all else must be both a symlink and resolve to valid location
          (File.symlink?(fullpath) and (File.file?(fullpath) or File.directory?(fullpath)))

      end
      # Some relx projects do not properly link system libraries -
      # so we have to make sure that *some* of our
      @loaded = files.length == 0
      return true if @loaded

      # Now our alternative check, based on naive dep linking:


      @loaded

    end

    def do_load(options)
      # TODO this can also be wrapped and handled in the base...
      if not project_dir_exists_on_host?(name)
        git = project['git']
        if git
          if git['uri']
            clone(name, git['uri'])
            if git['branch']
              checkout(name, git['branch'])
            end
          end
        else
          raise DVMArgumentError, "Project not available. Clone it onto your host machine, or update the git settings in config.yml."
        end
      end

      raise DVMArgumentError, "Project already loaded. Use --force to proceed." if loaded? and not options[:force]
      run_command("chef-server-ctl stop #{service['name']}", "Stopping #{service['name']}", cwd: project_dir)
      do_build unless options[:no_build]

      say("Setting up symlinks")
      FileUtils.rm_rf(["#{relpath}/log", "#{relpath}/sys.config"])
      FileUtils.ln_s("/var/opt/opscode/#{service["name"]}/sys.config", "#{relpath}/sys.config")
      FileUtils.ln_s("/var/log/opscode/#{service["name"]}", "#{relpath}/log")

      # Make runsv forget about us so that chef-server-ctl reconfigure doesn't restart.
      # Link may be missing if we're force-reloading
      #if (File.exists? "/opt/opscode/service/#{service['name']}")
        #FileUtils.rm("/opt/opscode/service/#{service['name']}")
      #end
      say(HighLine.color("Success! Your project is loaded.", :green))
      say("Start it now:")
      say(HighLine.color("    dvm start #{name} [--background]", :green))
    end

    def unload
      name = service['name']
      # NOt working - naturally ctl reconfigure  recreates this link.
      # Could be that we want to coordinate between projects in here,
      # reconfigure-notify: oc_erchef?
      # FileUtils.ln_s("/opt/opscode/service/#{name}", "/opt/opscode/sv/#{name}")
      FileUtils.rm("#{relpath}/sys.config")
      FileUtils.rm("#{relpath}/log")
      run_command("chef-server-ctl start #{name}", "Restarting packaged version of #{name} via chef-server-ctl", cwd: project_dir)
    end

    def do_build
      say("Cleaning deps...")
      FileUtils.rm_rf(["#{project_dir}/deps"])
      run_command("make clean relclean", "Cleaning up everything else",     cwd: project_dir)
      run_command("rebar get-deps -C rebar.config.lock",       "Getting all deps, please hold.", cwd: project_dir)

      concurrency = project_setting("build.max_concurrency") || 4
      # TODO erm, this env should be in from config, it's specific to oc_erchef...
      run_command("make devrel -j #{concurrency}",    "Building... ", cwd: project_dir, env: { "USE_SYSTEM_GECODE" => "1"})
    end

    def enable_cover(modules = nil)
      say "Disabling sync to prevent conflicts in loaded beam files."
      update("pause")
        run_command("#{erl_command} -eval \"rpc:call('#{node}', sync, go, []).\" -s erlang halt", "Resuming sync on #{node}")
        run_command("#{erl_command} -eval \"rpc:call('#{node}', sync, go, []).\" -s erlang halt", "Resuming sync on #{node}")
      say "Cover-compiling app beams."
      # Run these guys next - need to do it by hand via eval!
      eval_cmd = <<-EOM
F = fun() ->
  Dirs = file:wildcard("/host/#{project_name}/apps/*/ebin\") ++ ["/host/#{project_name}/ebin"]
  [cover:compile_beam_dir(Dir) || Dir <- Dirs]
end,
spawn(#{node}, F).
EOM
      run_command("#{erl_command} -eval \"#{eval_cmd}\" -s erlang halt", "Cover-compiling modules on the erlang node.")
    end
    def dump_cover(modules = nil)
      # TODO
    end
    def update(args)
      # No running check in case we're trying to attach to the global instance
      node = service['node']
      if args == "pause"
        pause_msg = "Pausing sync on #{node}. This will take effect after any running scans have been completed. \nUse 'dvm update oc_erchef resume' to re-enable automatic updates."
        run_command("#{erl_command} -eval \"rpc:call('#{node}', sync, pause, []).\" -s erlang halt", pause_msg)
      elsif args == "resume"
        run_command("#{erl_command} -eval \"rpc:call('#{node}', sync, go, []).\" -s erlang halt", "Resuming sync on #{node}")
      end
    end

    def etop
      exec "#{erl_command} -s etop -s erlang halt -output text -sort reductions -lines 25 -node #{service['node']}"
    end

    def erl_command
      "erl -hidden -name dvm@127.0.0.1 -setcookie #{service["cookie"]}"
    end

  end
end

