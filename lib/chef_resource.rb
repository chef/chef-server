module ChefResource
  def get
    attrs = chef.get_rest(url)
    set_attributes(attrs)
    self
  end

  def url
    raise NotImplementedError, 'A Chef resource URL (e.g., "users/applejack") is required.'
  end

  def chef
    ::Chef::REST.new endpoint, Settings.chef.superuser, nil, parameters
  end

  private

  def endpoint
    Settings.chef.endpoint
  end

  def headers
    { 'x-ops-request-source' => 'web' }
  end

  def key
    IO.read(File.expand_path(Settings.chef.key_path, __FILE__)).strip
  end

  def parameters
    { headers: headers, raw_key: key }
  end
end
