module DVM
  # Note that when 'system' is configured 'true', the project will
  # be overlaid into the /opt/opscode/embedded/lib/ruby/gems/X/$project directory
  # via bind mount.  This will ensure it's available for use in chef-server-ctl commands
  # which aren't run from in the dvm substitute environment.
  class RubyProject < Project
    def initialize(project_name, config)
      super
    end
    def do_load(options)
      if @project['system']
        load_system_ruby_project
      else
        load_ruby_project
      end
    end
    def load_system_ruby_project
      # For now we're not loading further gem deps - we can revisit that
      # if/when we have a need to.
      bind_mount(project_dir,   system_gem_path(name))
    end
    def load_ruby_project
      # ruby projects that can be run as commands will be updated with a new bundler file in place,
      # so that they can just be run via `dvm run #{name}` without having to mess around
      # with the gems installed in package ruby.
      run_command("rm -rf .bundle/config", cwd: project_dir)
      run_command("bundle install --path /opt/opscode/embedded/service/gem --no-binstubs", "Installing in-place...", cwd: project_dir)
    end
    def unload
      unmount(@project_dir)
    end
    def loaded?
      if project['system']
        path_mounted?(project_dir)
      else
        # No real way to say loaded or not.
        # Perhaps we could take a look at .bundle/config and see if it points to us?
        false
      end
    end
    def run(args)
      if @project['system']
        raise DVM::DVMArgumentError, 'Run not supported for system ruby projects - just use it normally via chef-server-ctl or otherwise, as it has been loaded into the server gemset.'
      else
        exec "cd #{@project_dir} && #{@project['run']} #{args.join(" ")}"
      end
    end
  end
end
