
require 'net/http'
require 'net/https'
require "redis"

require 'uri'

VALID_CHEF_VERSION = "11.6.0"

VALID_CHEF_VERSIONS = ["10.0.0", "10.0.1", "10.0.2", "10.0.9",
                       "10.1.0", "10.9.0", "0.10.0", "10.99.0",
                       "11.0.0", "11.1.0", "11.99.99", "11.9999.999"]

INVALID_CHEF_VERSIONS = ["0.9.0", "9.0.0", "9.9.9", "12.0.0", "12.99.99"]

EXT_HOST = "33.33.33.50"
INT_HOST = "33.33.33.100"

EXTERNAL_HTTPS = "https://#{EXT_HOST}"
EXTERNAL_HTTP = "http://#{EXT_HOST}"

INTERNAL_HTTPS = "https://#{INT_HOST}"
INTERNAL_HTTP = "http://#{INT_HOST}"

INTERNAL_CHEF = "http://#{INT_HOST}:6880"
INTERNAL_CERT = "http://#{INT_HOST}:5140"
INTERNAL_ACCT = "http://#{INT_HOST}:5760"

DEFAULT_ORG_NAME = "testorg"

REPORTING_PROTO_HEADER = "X-Ops-Reporting-Protocol-Version"

MY_IP = "33.33.33.1"
MY_24 = "33.33.33"

OTHER_IP = "160.1.72.1"
OTHER_24 = "160.1.72"

RSpec.configure do |config|
    config.treat_symbols_as_metadata_keys_with_true_values = true
    config.run_all_when_everything_filtered = true
    config.filter_run :focus
end

module SpecHelper
  attr_accessor :response, :body, :code
  def make_request(params)
    type = params[:type]
    location = params[:location] || valid_location_for(type, DEFAULT_ORG_NAME)
    version = params[:version] || VALID_CHEF_VERSION
    method = params[:method] || :get
    headers = params[:headers] || {}

    uri = URI.parse("#{server(type)}#{location}")
    http = Net::HTTP.new(uri.host, uri.port)
    req = request_for_method(method, uri)
    req.add_field("X-Chef-Version", version) unless params[:no_version]
    req.add_field("X-Ops-UserId", "test") unless params[:no_user]
    req.add_field("Content-Length", "0") if method == :put
    headers.each { |k, v| req.add_field(k, v) }
    if uri.port == Net::HTTP.https_default_port
      http.use_ssl = true
      # Local vagrant server won't verify...
      http.verify_mode = OpenSSL::SSL::VERIFY_NONE
    end
    @response = http.request(req)
    @body = @response.body
    @code = @response.code
  end

  def valid_route_for(type)
    case type
    when :intchef
      "erchef"
    when :intacct
      "acct"
    when :internal
      "erchef"
    when :external
      "reports"
    when :intcert
      raise "invalid - intcert does not accept routes"
    end
  end
  def valid_location_for(type, orgname)
    case type
    when :intchef
      "/organizations/#{orgname}/nodes"
    when :intacct
      "/organizations/#{orgname}/users"
    when :internal
      "/organizations/#{orgname}/nodes"
    when :external
      "/organizations/#{orgname}/reports"
    when :intcert
      "/anything/goes"
    end
  end
  def redis_inst(type)
    case type
    when :intchef
      Redis.new(:host => INT_HOST, :port => 6379)
    when :intacct
      Redis.new(:host => INT_HOST, :port => 6379)
    when :internal
      Redis.new(:host => INT_HOST, :port => 6379)
    when :intcert
      Redis.new(:host => INT_HOST, :port => 6379)
    when :external
      Redis.new(:host => EXT_HOST, :port => 6379)
    end
  end

  def set_org_blocked(redis, org_name)
    redis.hsetnx "dl_org_#{org_name}", "org_blocked", "true"
  end

  def clear_org_blocked(redis, org_name)
    redis.hdel "dl_org_#{org_name}", "org_blocked"
  end
  def set_org_maint_mode(redis, org_name)
    redis.hsetnx "dl_org_#{org_name}", "503_mode", "true"
  end

  def clear_org_maint_mode(redis, org_name)
    redis.hdel "dl_org_#{org_name}", "503_mode"
  end

  def set_route_maint_mode(redis, route_name)
    #TODO again, change to 503_mode_route_id
    redis.sadd "maint_data", "maint_mode_#{route_name}"
  end

  def clear_route_maint_mode(redis, route_name)
    #TODO again, change to 503_mode_route_id
    redis.srem "maint_data", "maint_mode_#{route_name}"
  end

  # TODO name as 503_mode for consistency
  def set_global_maint_mode(redis)
    redis.sadd "maint_data", "maint_mode"
  end

  def clear_global_maint_mode(redis)
    redis.srem "maint_data", "maint_mode"
  end

  def set_route_darklaunch(redis, route_name)
    redis.sadd "maint_data", "dl_#{route_name}"
  end

  def clear_route_darklaunch(redis, route_name)
    redis.srem "maint_data", "dl_#{route_name}"
  end

  def darklaunch_org_for_route(redis, org_name, route_name)
    redis.hsetnx "dl_org_#{org_name}", "dl_#{route_name}", "true"
  end

  def undarklaunch_org_for_route(redis, org_name, route_name)
    redis.hdel "dl_org_#{org_name}", "dl_#{route_name}"
  end

  def darklaunch_org_for_api(redis, org_name, route_name)
    redis.hsetnx "dl_org_#{org_name}", "couchdb_#{route_name}", "false"
  end

  def undarklaunch_org_for_api(redis, org_name, route_name)
    redis.hdel "dl_org_#{org_name}", "couchdb_#{route_name}"
  end
  def set_route_maint_mode(redis, route_name)
    redis.sadd "maint_data", "maint_mode_#{route_name}"
  end

  def clear_route_maint_mode(redis, route_name)
    redis.srem "maint_data", "maint_mode_#{route_name}"
  end

  def disable_new_org_creation(redis)
    redis.hsetnx "dl_org__OC_INTERNAL_NO_ORG", "disable_new_orgs", "true"
  end

  def enable_new_org_creation(redis)
    redis.hdel "dl_org__OC_INTERNAL_NO_ORG", "disable_new_orgs"
  end

  def tarpit_upstream_for_org(redis, upstream, org_name, tarpit_id)
    redis.hsetnx "dl_org_#{org_name}", "tarpit_#{upstream}", tarpit_id
  end

  def detarpit_upstream_for_org(redis, upstream, org_name)
    redis.hdel "dl_org_#{org_name}", "tarpit_#{upstream}"
  end

  def clear_all_defaults(redis)
    redis.del "dl_default"
  end

  def clear_all_for_org(redis, org_name)
    redis.del "dl_org_#{org_name}"
  end

  # TODO perhas a cleaner option here, 'whitelist_IP" for clarity when looking at values?
  def whitelist(redis, ip)
    redis.sadd "maint_data", ip
  end

  def dewhitelist(redis, ip)
    redis.srem "maint_data", ip
  end

  def ban(redis, ip)
    redis.sadd "banned_ips", ip
  end

  def unban(redis, ip)
    redis.srem "banned_ips", ip
  end

  def request_for_method(method, uri)
    case method
    when :post
      Net::HTTP::Post.new(uri.request_uri)
    when :get
      Net::HTTP::Get.new(uri.request_uri)
    when :delete
      Net::HTTP::Delete.new(uri.request_uri)
    when :put
      Net::HTTP::Put.new(uri.request_uri)
    end
  end

  def server(type)
    case type
    when :external
      EXTERNAL_HTTPS
    when :external_http
      EXTERNAL_HTTP
    when :internal
      INTERNAL_HTTPS
    when :internal_http
      INTERNAL_HTTP
    when :intacct
      INTERNAL_ACCT
    when :intchef
      INTERNAL_CHEF
    when :intcert
      INTERNAL_CERT
    end
  end

  # our stubbed upstreams will respond back to us with the original relative
  # path that we provided in the request, but it will be stripped of parameters
  # and normalized in form - double-slashes (where accepted) will be removed.
  def strip_location(location)
    content = location.split('?')[0]
    content.gsub(/((\/)+)+/, '/')
  end
end

