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

      # If we have not been bootstrapped and are using chef-backend,
      # we won't be able to use the standard ha-proxy URL. In this case, we'll
      # hit the cluster members directly.
      if !OmnibusHelper.has_been_bootstrapped?  && ChefBackend.enabled?(node)
        port = node['private_chef']['haproxy']['remote_elasticsearch_port']
        puts "*** #{ChefBackend.members(node)}"
        ChefBackend.members(node).each do |_, host|
          @es_version = try_version_from_server("http://#{host}:#{port}")
          break if @es_version
        end
      else
        # If we're using a vanilla external ES, it's expected to be up and running for us
        # and reachable at the configured url.
        @es_version = try_version_from_server(node['private_chef']['opscode-solr4']['external_url'])
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
    puts "**** Could not get version from #{url}: #{e}"
    false
  end
end

