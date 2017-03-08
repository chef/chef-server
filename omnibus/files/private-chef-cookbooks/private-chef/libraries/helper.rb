require 'mixlib/shellout'
require 'uri'

class OmnibusHelper
  attr_reader :node

  def initialize(node)
    @node = node
  end

  def ownership
    owner = node['private_chef']['user']['username']
    group = owner

    {"owner" => owner, "group" => group}
  end

  def rabbitmq_configuration
    external = node['private_chef']['external-rabbitmq']['enable']
    config = if external
               node['private_chef']['external-rabbitmq'].to_hash
             else
               node['private_chef']['rabbitmq'].to_hash
             end

    # TODO(ssd) 2017-03-07: We need to look into how analytics integrates with the chef-server
    # to figure out how this credential flows through the system.
    config['actions_password'] = PrivateChef.credentials.get("rabbitmq", "actions_password")
    config
  end

  # Normalizes hosts. If the host part is an ipv6 literal, then it
  # needs to be quoted with []
  def self.normalize_host(host_part)
    # Make this simple: if ':' is detected at all, it is assumed
    # to be a valid ipv6 address. We don't do data validation at this
    # point, and ':' is only valid in an URL if it is quoted by brackets.
    if host_part =~ /:/
      "[#{host_part}]"
    else
      host_part
    end
  end

  def normalize_host(host_part)
    self.class.normalize_host(host_part)
  end

  def vip_for_uri(service)
    normalize_host(node['private_chef'][service]['vip'])
  end

  # Returns scheme://host:port without any path
  def solr_root
    url = URI.parse(solr_url)
    host = url.scheme + "://" + url.host
    if url.port
      host += ":" + url.port.to_s
    end
    host
  end

  def solr_url
    if node['private_chef']['opscode-solr4']['external']
      node['private_chef']['opscode-solr4']['external_url']
    else
      "http://#{vip_for_uri('opscode-solr4')}:#{node['private_chef']['opscode-solr4']['port']}/solr"
    end
  end

  def bookshelf_s3_url
    # Using URI#to_s to strip ":443" for https and ":80" for http
    URI("#{node['private_chef']['nginx']['x_forwarded_proto']}://#{vip_for_uri('bookshelf')}:#{node['private_chef']['bookshelf']['vip_port']}").to_s
  end

  def nginx_ssl_url
    # Using URI#to_s to strip ":443" for https and ":80" for http
    URI("#{node['private_chef']['nginx']['url']}:#{node['private_chef']['nginx']['ssl_port']}").to_s
  end

  def db_connection_uri
    db_protocol = "postgres"
    db_user     = node['private_chef']['opscode-erchef']['sql_user']
    db_password = node['private_chef']['opscode-erchef']['sql_password']
    db_vip      = vip_for_uri('postgresql')
    db_name     = "opscode_chef"

    "#{db_protocol}://#{db_user}:#{db_password}@#{db_vip}/#{db_name}"
  end

  def bifrost_db_connection_uri
    db_protocol = "postgres"
    db_user     = node['private_chef']['oc_bifrost']['sql_user']
    db_password = node['private_chef']['oc_bifrost']['sql_password']
    db_vip      = vip_for_uri('postgresql')
    db_name     = "bifrost"

    "#{db_protocol}://#{db_user}:#{db_password}@#{db_vip}/#{db_name}"
  end

  # This file is touched once initial bootstrapping of the system is
  # done.
  def self.bootstrap_sentinel_file
    "/var/opt/opscode/bootstrapped"
  end

  # Use the presence of a sentinel file as an indicator for whether
  # the server has already had initial bootstrapping performed.
  #
  # @todo: Is there a more robust way to determine this, i.e., based
  #   on some functional aspect of the system?
  def self.has_been_bootstrapped?
    File.exists?(bootstrap_sentinel_file)
  end

  # Parse a config string as a memory value returning an integer in MB
  # units.  Supported inputs (not case sensitive) are B, K/Kb, M/Mb,
  # G/Gb. Uses integer division so values in B and Kb must exceed 1Mb.
  def self.parse_mem_to_mb(mem_str)
    if mem_str.is_a?(Integer)
      return mem_str
    end
    regex = /(\d+)([GgmMkKbB]{0,2})/
    m  = regex.match(mem_str)
    raise "bad arg" if !m
    raw_value = m[1].to_i
    units = m[2]
    value = case units
            when /^b$/i
              raw_value / (1024 * 1024)
            when /^kb?$/i
              raw_value / 1024
            when /^mb?$/i
              raw_value
            when ""                       # no units, assume Mb
              raw_value
            when /^gb?$/i
              raw_value * 1024
            else
              raise "unsupported memory units: #{mem_str}"
            end
    if value > 0
      value
    else
      raise "zero Mb value not allowed: #{mem_str}"
    end
  end

  def self.erl_atom_or_string(term)
    case term
    when Symbol
      term
    when String
      "\"#{term}\""
    else
      "undefined"
    end
  end

  def erl_atom_or_string(term)
    self.class.erl_atom_or_string(term)
  end

  def self.escape_characters_in_string(string)
    return "" unless string.is_a? String
    pattern = /(\'|\"|\.|\*|\/|\-|\\)/
    string.gsub(pattern){|match|"\\"  + match}
  end

  def escape_characters_in_string(string)
    self.class.escape_characters_in_string(string)
  end

  def s3_url_caching(setting)
    case setting.to_s
    when "off"
      "off"
    when /m$/
      "{#{setting.chop}, minutes}"
    when /%$/
      "{#{setting.chop}, percent}"
    end
  end

  # OC-11540, fallback to ssl_port if non_ssl_port is disabled
  def internal_lb_url
    if node['private_chef']['nginx']['non_ssl_port'] == false
      "https://#{vip_for_uri('lb_internal')}:#{node['private_chef']['nginx']['ssl_port']}"
    else
      "http://#{vip_for_uri('lb_internal')}:#{node['private_chef']['nginx']['non_ssl_port']}"
    end
  end

  def ldap_authentication_enabled?
    node['private_chef'].attribute?('ldap') &&
      !(node['private_chef']['ldap'].nil? || node['private_chef']['ldap'].empty?)
  end

  def platform_package_suffix
    case node['platform_family']
    when 'debian'
      'deb'
    when 'rhel', 'suse'
      'rpm'
    else
      # TODO: probably don't actually want to fail out?
      raise "I don't know how to install addons for platform family: #{node['platform_family']}"
    end
  end

  def remote_install_addons?
    # chef-solo and chef-client -z return different things :(
    (node['private_chef']['addons']['path'] == nil) || (node['private_chef']['addons']['path'] == {})
  end
end
