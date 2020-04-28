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
    ::Chef::Config.ssl_verify_mode Settings.chef.ssl_verify_mode.to_sym || :verify_peer
    ::Chef::ServerAPI.new endpoint, parameters
  end

  private

  def endpoint
    Settings.chef.endpoint
  end

  def headers
    { 'x-ops-request-source' => 'web' }
  end

  def key
    Secrets.get("#{Chef::Dist::Server::SHORT}", "webui_key")
  end

  def parameters
    { headers: headers,
      client_name: Settings.chef.superuser,
      client_key: nil,
      api_version: "0",
      raw_key: key }
  end
end
