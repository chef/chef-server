# Interesting to note that thor and highliine are already in our
# omnibus bundle.
require "thor"
require "highline/import"
module DVM
  PROJECT_CLASSES = {
    # TODO module Project, class method 'base_name', enumerate and generate...
    "omnibus" => DVM::OmnibusProject,
    "erlang" => DVM::ErlangProject,
    "ruby" => DVM::RubyProject
  }
  class AppBase < Thor
    def initialize(args, local_options,config)
      super
      # Yeah, sorry - it just made sense to have a common
      # global init to load config data shared across class instances that I don't
      # control the instantiation of...
      @config = $config
      @projects = {}
      @config["projects"].each do |name, project|
        type = project["type"]
        @projects[name] = PROJECT_CLASSES[type].new(name, @config)
      end
    end
    protected

    def ensure_project(name)
      raise DVM::DVMArgumentError, "No such project: #{name}" unless @projects.has_key? name
      @projects[name]
    end
    def ensure_erlang_project(name)
      p = ensure_project(name)
      raise DVM::DVMArgumentError, "This command is only supported for erlang projects.  #{name} is a #{p.type}" unless /.*erlang.*/ =~ p.type
      p

    end
    def ensure_dep(project, dep)
      raise DVM::DVMArgumentError, "No such dep #{dep} for project #{project}" unless project.ensure_dep(dep)
    end
  end

  class Application < AppBase
    desc "ls [project]", "list available projects, or available dependencies within project"
    def ls(project = nil)
      list(project)
    end

    desc "list [project]", "list available projects, or available dependencies within project"
    def list(project = nil)
      if project == nil
        @projects.each do |name, p|
          say("#{HighLine.color(name, :green)} #{p.loaded? ? "loaded" : "not_loaded"}")
        end
      else
        p = ensure_project(project)
        say(HighLine.color("#{project} deps:", :bold))
        p.deps.each do |name, dep|
          status, c = dep.loaded? ? ["loaded", :green] : (dep.available? ? ["available", :blue]  : ["not available", :white])
          say("  #{name}: #{HighLine.color(status, c)}")
        end
      end
    end

    option :no_build, type: :boolean,
                        aliases: ['-n'],
                        desc: "skip the build phase and just load/mount the source path"
    option :force, type: :boolean,
                   aliases: ['-f'],
                   desc: "force the load even if the component is already loaded"
    option :auto_clone, type: :boolean,
                   aliases: ['-a'],
                   desc: "(Erlang Only, Dependency Only) if the dependency can't be found, clone it into external-deps on the host automatically"
    desc "load <project> [dep-or-sub]", "load a project or project's named dependency"
    def load(project_name, dep = nil)
      project = ensure_project(project_name)
      if dep.nil?
        project.load(options)
      else
        ensure_dep(project, dep)
        project.load_dep(dep, options)
      end
    end


    desc "etop <project>", "(Erlang) run etop in an erlang console to monitor the running project. Project must have etop available."
    def etop(project_name)
      ensure_erlang_project(project_name).etop
    end

    desc "unload <project> [dep]", "unload a project or a project's named dependency"
    def unload(project_name, dep = nil)
      p = ensure_project(project_name)
      if dep.nil?
        p.unload()
      else
        ensure_dep(project, dep)
        p.unload_dep(dep)
      end
    end

    desc "updates <project> <pause|resume>",  "(Erlang Only) For projects with sync support, pause or resume updates in the running instance."
    def updates(project_name, action)
      ensure_erlang_project(project_name).update(project)
    end

    # Interestingly enough, this passes through successfully if you just use it as 'run' -
    # perhaps matched on prefix?
    desc "runit <project> [run-args]", "Run a non-service project"
    def runit(project_name, *args)
      args = args.nil? ? [] : args
      ensure_project(project_name).run args
    end

    desc "start <project> [run-args]", "(Erlang) Start the service associated with the project, out of its location in /host.  Erlang: Will remain in foreground as a running console unless --background is specified."
    option :background, type: :boolean,
                        aliases: ['-b'],
                        desc: "(Erlang) Launch the service in the background instead of the default behavior of bringing it to the foreground for interactive use"
    def start(project_name, *args)
      args = args.nil? ? [] : args
      ensure_project(project_name).start args, options[:background]
    end

    desc "console <project>", "(Erlang) connect to a running process in a console. This will connect to the original installed service or the one running out of /host - whichever is running."
    def console(project_name)
      ensure_erlang_project(project_name).console
    end

    desc "stop <project>", "Stop a running service that has been loaded out of /host"
    def stop(project_name)
      ensure_erlang_project(project_name).stop
    end

    desc "quickstart (or qs) [<configname>]", "Execute a quickstart configuration.  Use with no arguments to see what's available"
    def qs(configname = nil)
      quickstart(configname)
    end

    desc "quickstart (or qs) [<configname>]", "Execute a quickstart configuration.  Use with no arguments to see what's available"
    option :dryrun,      type: :boolean,
                         aliases: ['-n'],
                         default: false,
                         desc: "Just show what would be done instead of actually doing it."
    def quickstart(configname = nil)
      if configname.nil?
        say "The following quickstart configurations are available:"
        @config["quickstart"].each do |name, qs|
          say "  #{name} : #{qs["description"]}"
        end
      else
        qs = @config["quickstart"][configname]
        raise DVMArgumentError, "No such quickstart configuration found: #{configname}" if qs.nil?
        puts qs["description"] if qs.has_key? "description"
        if qs.has_key? "load"
          qs["load"].each do |project_info|
            name, args = project_info.split(" ", 2)
            if options[:dryrun]
              say " * would: load: #{project_info}"
              load(name, args)
            else
              say " * Loading: #{name}"
              load(name, args)
            end
          end
        end
        if qs.has_key? "start"
          qs["start"].each do |project_info|
            args = project_info.split(" ")
            if options[:dryrun]
              say " * would: start #{project_info}"
            else
              DVM::Application.start(["start"] + args)
            end

          end
        end
      end
    end


    class Cover < AppBase
      desc "enable PROJECT [MODULE] [--with-deps|-d]", "Enable code coverage for MODULE within PROJECT, or all modules if none is given."
      option :with_deps, type: :boolean, aliases: ['-d'], default: false,
              desc: "Enable coverage for all project dependencies too.  Only has effect when MODULE is not provided."
      long_desc <<EOM
      Enable code coverage for MODULE within PROJECT, or for all modules in the project if none
      is specified.   The name module can be either in the project itself, or in a dependency of the project.

      When code coverage is enabled, automatic hot loading of changes is disabled to prevent unloading instrumented code.
EOM
      def enable(projectname, modulename = 'all')
        ensure_erlang_project(projectname).cover_enable(modulename, options)
      end

      desc "disable PROJECT",
           "Disable all code coverage monitoring and re-enable hot loading of changes if supported in the project."
      def disable(projectname)
        ensure_erlang_project(projectname).cover_disable()
      end

      option :raw,  type: :boolean, aliases: ['-r'], default: false,
                    desc: "generate raw coverage reports instead of HTML"
      desc "report PROJECT [MODULE] [--raw|-r]", "Generate coverage reports"
      long_desc <<EOM
      Generate coverage reports based on all currently monitored modules in PROJECT, or for a specifically
      named module. If raw is specified, output will be raw data. Otherwise it will be HTML-formatted.

      The reports will be output to appropriate project directories within the directory on the guest as specified under
      vm.cover.base_output_path in defaults.yml or config.yml .
EOM
      def report(projectname, modulename = 'all')
        ensure_erlang_project(projectname).cover_report(modulename, options)
      end

      desc "reset PROJECT [MODULE]", "Reset coverage stats for PROJECT, or MODULE within PROJECT"
      long_desc <<EOM
      Reset coverage stats for MODULE within PROJECT, or for all modules in the project if no
      module is specified. Does not otherwise stop or enable coverage monitoring."
EOM
      def reset(projectname, modulename = 'all')
        ensure_erlang_project(projectname).cover_reset(modulename, options)

      end
    end

    desc "cover SUBCOMMAND ...ARGS", "manage runtime code coverage for Erlang projects."
    subcommand("cover", Cover)


  end
end
