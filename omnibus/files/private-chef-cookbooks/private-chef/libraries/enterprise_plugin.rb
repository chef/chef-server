class EnterprisePlugin
  def initialize(name)
    @name = name
    @enabled_by_default = false
  end

  def set_or_return(name, value = nil)
    if !value.nil?
      instance_variable_set("@#{name}", value)
    else
      instance_variable_get("@#{name}")
    end
  end

  def name(value = nil)
    set_or_return(:name, value)
  end

  def parent_plugin(value = nil)
    set_or_return(:parent_plugin, value)
  end

  def cookbook_path(value = nil)
    set_or_return(:coobook_path, value)
  end

  def enabled_by_default(value = nil)
    set_or_return(:enabled_by_default, value)
  end

  def enabled(value = nil)
    set_or_return(:enabled, value)
  end

  def definition_location(value = nil)
    set_or_return(:definition_location, value)
  end

  def config_extension_path(value = nil)
    set_or_return(:config_extension_path, value)
  end

  def enabled_run_list
    "recipe[#{name}::enable]"
  end

  def disabled_run_list
    "recipe[#{name}::disable]"
  end

  def run_list
    if enabled?
      enabled_run_list
    else
      disabled_run_list
    end
  end

  def enabled?
    if @enabled.nil?
      enabled_by_default
    else
      @enabled
    end
  end
end
