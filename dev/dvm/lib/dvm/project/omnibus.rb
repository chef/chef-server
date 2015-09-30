require_relative "omnibus_dep"

module DVM
  class OmnibusProject < Project
    def initialize(project_name, config)
      super
    end
    def parse_deps()
      #  for us, deps are more subprojects.
      if @deps.nil?
        @deps = {}
        @project['components'].each do |sub, c|
          @deps[sub] = OmnibusDep.new(project_dir, sub, c)
        end
      end
    end
    def loaded?
      File.directory? @project_dir
    end
    def do_load(ignore)
      # TODO we could actually add the standard clone-if-missing support
      say "This project is not loadable on its own. Instead use 'dvm load #{name} $subproject"
    end
    def do_unload
      say "This project is not unloadable on its own. Instead use `dvm unload #{name} $subproject"
    end
  end
end
