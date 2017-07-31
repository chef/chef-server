require 'chef/http'

class EsHelper
  # Returns a Gem::Version object with the elasticsearch server version. Obtains
  # version via ES REST API
  # If the search provider is not elasticsearch, it returns an instance for
  # v0.0.0
  def self.es_version(node)
    if node['private_chef']['opscode-erchef']['search_provider'] == 'solr'
      return Gem::Version.new('0.0.0')
    end
    server_url = node['private_chef']['opscode-solr4']['external_url']
    server = Chef::HTTP.new(server_url)
    raw = server.get('')
    server_info = JSON.parse(raw)
    version = server_info['version']['number']
    Gem::Version.new(version)
  end
end
