module DVM
  class RubyProject < Project
    def initialize(project_name, config)
      super
    end
    def do_load(options)
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
end
