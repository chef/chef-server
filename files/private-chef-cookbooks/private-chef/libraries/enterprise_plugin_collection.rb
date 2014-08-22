class EnterprisePluginCollection

  attr_reader :plugins
  def initialize
    @plugins = []
  end

  def plugin(name, &block)
    p = EnterprisePlugin.new(name)
    p.instance_eval &block
    @plugins << p
  end

  def load_from_file(filename)
    nc = EnterprisePluginCollection.new
    nc.instance_eval(::File.read(filename))
    nc.plugins.each do |p|
      p.cookbook_path "#{::File.dirname(filename)}/embedded/cookbooks" unless p.cookbook_path
    end
    @plugins += nc.plugins
  end

  # it is a bit gross that we just return
  # the array rather than the real object
  def self.from_file(filename)
    pc = EnterprisePluginCollection.new
    pc.load_from_file(filename)
    pc.plugins
  end

  def self.from_glob(glob)
    c = new()
    Dir.glob(glob) do |filename|
      c.load_from_file(filename)
    end
    c.plugins
  end
end
