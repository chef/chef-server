require 'json'
require 'uri'
require 'chef/http'

module ChefBackend
  ETCD_MEMBERS_URL = "/v2/members"
  def self.enabled?(node)
    node['private_chef']['use_chef_backend']
  end

  def self.members(node)
    return [] unless ChefBackend.enabled?

    if @members.nil?
      node['private_chef']['chef_backend_members'].each do |member|
        begin
          @members = ChefBackend.etcd_members(member, node['private_chef']['haproxy']['etcd_port'])
          break if @members && !@members.empty?
        rescue StandardError => e
          Chef::Log.warn("Error attempting to get cluster members from #{member}:")
          Chef::Log.warn("  #{e}")
          Chef::Log.info("Trying next configured chef_backend member.")
        end
      end
    end
  rescue StandardError => e
    Chef::Log.warn("member discovery failed: #{e}")
  ensure
    if @members.nil? || @members.empty?
      Chef::Log.warn("Using statically configured member list")
      @members = ChefBackend.configured_members(node)
    end
    return @members
  end

  def self.configured_members(node)
    ret = {}
    node['private_chef']['chef_backend_members'].each_with_index do |member, i|
      ret["backend#{i}"] = member
    end
    ret
  end

  def self.etcd_members(ip, port)
    ret = {}
    raw_members = JSON.parse(etcd_get(ETCD_MEMBERS_URL, ip, port))["members"]
    raw_members.each do |m|
      ret[m["name"]] = URI.parse(m["peerURLs"].first).host
    end
    ret
  end

  def self.etcd_get(url, ip, port)
    client = Chef::HTTP.new("http://#{ip}:#{port}")
    client.get(url)
  end
end
