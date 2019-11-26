require 'json'
require 'uri'
require 'chef/http'

module ChefBackend
  ETCD_MEMBERS_URL = '/v2/members'.freeze
  def self.configured_members(node)
    ret = {}
    node['private_chef']['chef_backend_members'].each_with_index do |member, i|
      ret["backend#{i}"] = member
    end
    ret
  end

  def self.etcd_members(ip, port)
    ret = {}
    raw_members = JSON.parse(etcd_get(ETCD_MEMBERS_URL, ip, port))['members']
    raw_members.each do |m|
      ret[m['name']] = URI.parse(m['peerURLs'].first).host
    end
    ret
  end

  def self.etcd_get(url, ip, port)
    client = Chef::HTTP.new("http://#{ip}:#{port}")
    client.get(url)
  end
end
