require "mixlib/shellout"
module DVM
  class Project
    include DVM::Tools
    attr_reader :name, :project, :config, :service, :project_dir
    # TODO check required fields in config
    def initialize(project_name, config)
      @project = config['projects'][project_name]
      @name = project['name'].nil? ? project_name : project['name']
      @config = config
      @service = @project['service']
      @project_dir = "/host/#{name}"
    end
    def start(args, detach)
      raise DVM::DVMArgumentError, "Start not supported for #{name}"
    end

    def run(args)
      raise DVM::DVMArgumentError, "Run not supported for #{name}"
    end

    def database
      raise DVM::DVMArgumentError, "No database configured for #{name}" unless project['database']
      project['database']
    end

    def deps
      parse_deps
      @deps
    end

    def load(build)
      do_load(build)
    end
    def do_load(build)
    end

    def unload
      do_unload
    end
    def do_unload
      raise "You should implement this now."
    end

    def loaded?
      false
    end



    def load_dep(name, ignored_for_now)
      raise DVM::DVMArgumentError, "Load the project before loading deps." unless loaded?
      dep = ensure_dep(name)
      dep.load(ignored_for_now)
    end

    def unload_dep(name)
      raise DVM::DVMArgumentError, "Load the project before loading deps." unless loaded?
      dep = ensure_dep(name)
      raise DVM::DVMArgumentError, "#{name} is not loaded." unless dep.loaded?
      deps.unload
    end

    def ensure_dep(name)
      parse_deps
      raise DVM::DVMArgumentError, "This project does not have the dep #{name}" unless deps.has_key? name
      deps[name]
    end

    def parse_deps
      @deps = {}
    end

    # Simple helper to safely get nested hash elements.
    # Frankly I like this better than custom default_proc,
    # because it's less typing to do "a.b.c" than ["a"]["b"]["c"]
    def project_setting(path)
      keys = path.split(".")
      top = project
      # TODO
      keys.each do |key|
        if not top.nil? and top.has_key? key
          top = top[key]
        else
          return nil
        end
      end
      top
    end
    # so far these only apply to erlang projects. Maybe we should method_missing this stuff...
    def etop
      raise DVM::DVMArgumentError, "This application does not support etop."
    end

    def update(args)
      raise DVM::DVMArgumentError, "This project does not support dynamic updates."
    end

    def console
      raise DVM::DVMArgumentError, "This project does not support a console."
    end

  end
end
