require_relative "omnibus_dep"

module DVM
  class OmnibusProject < Project
    attr_reader :embedded_in

    def initialize(project_name, config)
      super
      # Omnibus projects now live with their corresponding project.
      # Change the default paths to reflect that.
      @embedded_in = project['embedded_in']
      if embedded_in
        @name = project['name'] || embedded_in
        if external
          @path = project['path'] || "external-deps/#{embedded_in}/omnibus"
        else
          @path = project['path'] || "src/#{embedded_in}/omnibus"
        end
        @project_dir = "/host/#{path}"
      end
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
