require 'chef/http'

class EsHelper
  # Returns a Gem::Version object with the elasticsearch server version. Obtains
  # version via ES REST API. If the search provider is not elasticsearch, it returns an instance for
  # v0.0.0.
  # If using chef-backend and bootstrap has not been done, it will query
  # cluster members diirectly until it finds a running instance that gives it a version
  def self.es_version(node)
    return @es_version if @es_version
    if node['private_chef']['opscode-erchef']['search_provider'] == 'solr'
      @es_version = Gem::Version.new('0.0.0')
      return @es_version
    else
      @es_version = try_version_from_server(node['private_chef']['opscode-solr4']['external_url'])
      unless @es_version
        # If we're using chef-backend that means our external url is via haproxy
        # In some situations (pre-bootstrap, reconfigure after stop) haproxy will not be available
        # at run time. In this case, we'll
        # hit the cluster members directly.
        if ChefBackend.enabled?(node)
          port = node['private_chef']['haproxy']['remote_elasticsearch_port']
          ChefBackend.members(node).each do |_, host|
            @es_version = try_version_from_server("http://#{host}:#{port}")
            break if @es_version
          end
        end
      end
    end
    @es_version
  end

  def self.try_version_from_server(url)
    server = Chef::HTTP.new(url)
    raw = server.get('')
    server_info = JSON.parse(raw)
    version = server_info['version']['number']
    Gem::Version.new(version)
  rescue => e
    Chef::Log.warn("Could not get version from #{url}")
    false
  end
end

