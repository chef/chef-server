# Interesting to note that thor and highliine are already in our
# omnibus bundle.
require "thor"
require "highline/import"
require "chef/mixin/deep_merge"
require "yaml"
module DVM
  PROJECT_CLASSES = {
    # TODO module Project, class method 'base_name', enumerate and generate...
    "omnibus" => DVM::OmnibusProject,
    "erlang" => DVM::ErlangProject,
    "ruby" => DVM::RubyProject,
    "rails" => DVM::RailsProject
  }
  class Application < Thor
    def initialize(args, local_options,config)
      super

      @projects = {}
      # Note use of hard-coded paths here.  Since I want this installed
      # as a gem, and to accept modifications at any time to the config files,
      # and this is intended for use ina  controlled environment - this seems
      # like the best answer.
      @config = YAML.load_file("/vagrant/defaults.yml")
      if File.file? "/vagrant/config.yml"
        overrides = YAML.load_file("/vagrant/config.yml")
        @config = Chef::Mixin::DeepMerge.deep_merge!(@config, overrides)
      end
      @config["projects"].each do |name, project|
        type = project["type"]
        @projects[name] = PROJECT_CLASSES[type].new(name, @config)
      end
    end



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
        say(HighLine.color("#{project} deps:", :bold))
        project = @projects[project]
        project.deps.each do |name, dep|
          status, c = dep.loaded? ? ["loaded", :yellow] : (dep.available? ? ["available", :green]  : ["not available", :red])
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
    option :skip_checkout, type: :boolean,
                   aliases: ['-s'],
                   desc: "do not attempt to clone the repo if it's missing, and do not change branches. "
    desc "load <project> [dep-or-sub]", "load a project or project's named dependency"
    def load(project_name, dep = nil)
      ensure_project(project_name)
      project = @projects[project_name]
      if dep.nil?
        project.load(options)
      else
        project.load_dep(dep, options)
      end
    end


    desc "etop <project>", "run etop to monitor the running project"
    def etop(project_name)
      ensure_project(project_name)
      @projects[project_name].etop

    end

    desc "unload <project> [dep]", "unload a project or a project's named dependency"
    def unload(project_name, dep = nil)
      ensure_project(project_name)
      if dep.nil?
        @projects[project_name].unload()
      else
        @projects[project_name].unload_dep(dep)
      end

    end

    # TODO this could be split into subcommands ...
    option :with_deps, type: :boolean,
                       aliases: ['-d'],
                       default: false,
                       desc: "when enabling coverage for all modules, also include deps"
    option :raw_output,  type: :boolean,
                         aliases: ['-r'],
                         default: false,
                         desc: "generate raw coverage reports. HTML is the default. Use with 'report'"
    desc "cover <project> <start|stop|report|reset> [modulename] [-r -d]", "manage runtime code coverage"
    def cover(project_name, action, modulename = "all")
      ensure_project(project_name)
      @projects[project_name].cover(action, modulename, options)
    end

    desc "update <project> <pause|resume>",  "if the  project supports it, pause or resume sync updates in the running instance"
    def update(project_name, action)
      ensure_project(project_name)
      @projects[project_name].update(action)
    end

    desc "runit <project> [run-args]", "Run the project"
    def runit(project_name, *args)
      ensure_project(project_name)
      args = args.nil? ? [] : args
      @projects[project_name].run args
    end

    desc "start <project> [run-args]", "Start the service associated with the project, out of the mounted directory. Currently supported for erlang and rails projects only.  Will remain in foreground unless --background is specified."
    option :"background", type: :boolean,
                        aliases: ['-b'],
                        desc: "Launch the server in the background instead of the default behavior of bringing it to the foreground for interactive use"
    def start(project_name, *args)
      ensure_project(project_name)
      args = args.nil? ? [] : args
      @projects[project_name].start args, options[:background]
    end

    desc "console <project>", "connect to a running process in a console if the projects supports it and the project service has been started."
    def console(project_name)
      ensure_project(project_name)
      @projects[project_name].console
    end

    desc "psql <project>", "connect to the database for a project, if it exists"
    def psql(project_name)
      ensure_project(project_name)
      database = @projects[project_name].database
      #exec "sudo -u opscode-pgsql /opt/opscode/embedded/bin/psql #{database}"
      exec "sudo -u opscode-pgsql /opt/opscode/embedded/bin/psql \"dbname=#{database} sslmode=require\""
    end


    desc "stop <project>", "Stop a running service that has been loaded locally"
    def stop(project_name)
      ensure_project(project_name)
      @projects[project_name].stop
    end


    desc "quickstart [<configname>]", "Execute a quickstart configuration.  Use with no arguments to see what's available"
    def qs(configname = nil)
      quickstart(configname)
    end
    desc "quickstart [<configname>]", "Execute a quickstart configuration.  Use with no arguments to see what's available"
    def quickstart(configname = nil)
      if configname.nil?
        say "The following quickstart configurations are available:"
        @config["quickstart"].each do |name, qs|
          say "  #{name} : #{qs["description"]}"
        end
      else
        # TODO support different quickstart configurations by name (bookshelf, oc-id, etc)
        qs = @config["quickstart"][configname]
        raise DVMArgumentError, "No such quickstart configuration found: #{configname}" if qs.nil?
        puts qs["description"] if qs.has_key? "description"
        if qs.has_key? "load"
          qs["load"].each do |project_info|
            name, args = project_info.split(" ", 2)
            puts "Loading: #{name}"
            load(name, args)
          end
        end
        if qs.has_key? "start"
          qs["start"].each do |project_info|
            args = project_info.split(" ")
            DVM::Application.start(["start"] + args)
          end
        end
      end
    end

    desc "populate", "Create users and orgs as defined in defaults & config yml"
    def populate


    end
  private
    def ensure_project(name)
      raise DVM::DVMArgumentError, "No such project: #{name}" unless @projects.has_key?(name)
    end

    def ensure_dep(project, dep)
      raise DVM::DVMArgumentError, "No such dep #{dep} for project #{project}" unless @projects[name].has_dep(dep)
    end
  end

end
