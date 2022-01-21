require 'mixlib/shellout'
require 'ipaddr'
require 'uri'
require 'net/http'

class OmnibusHelper
  attr_reader :node

  def initialize(node)
    @node = node
  end

  def ownership
    owner = node['private_chef']['user']['username']
    group = owner

    { 'owner' => owner, 'group' => group }
  end

  def apr1_password(password)
    cmd = Mixlib::ShellOut.new("openssl passwd -apr1 '#{password}'")
    cmd.run_command
    unless cmd.status.success?
      raise 'Failed to generate apr1 password hash'
    end

    cmd.stdout
  end

  def self.is_ip?(addr)
    IPAddr.new addr
    true
  rescue IPAddr::Error
    false
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

  def search_provider
    node['private_chef']['opscode-erchef']['search_provider']
  end

  def search_engine_auth_header
    if search_provider == 'opensearch'
      auth = Base64.strict_encode64("admin:admin")
      {Authorization: "Basic #{auth}"}
    else
      {}
    end
  end

  # def opensearch_password
  #   auth = Base64.strict_encode64("admin:admin")
  #   {Authorization: "Basic #{auth}"}
  # end

  def search_engine_major_version
    max_requests = 5
    current_request = 1

    if search_provider != 'opscode-solr4'
      begin
        client = Chef::HTTP.new(search_engine_url)
        response = client.get('', search_engine_auth_header)
      rescue => e
        # Perform a blind rescue because Net:HTTP throws a variety of exceptions - some of which are platform specific.
        if current_request == max_requests
          raise "Failed to connect to #{search_provider} service at #{search_engine_url}: #{e}"
        else
          # Chef HTTP logs the details in the debug log.
          Chef::Log.error "Failed to connect to #{search_provider} service #{current_request}/#{max_requests}. Retrying."
          current_request += 1
          sleep(current_request * 2) # Exponential back-off.
          retry
        end
      end
      begin
        version = JSON.parse(response)['version']['number'].split('.').first.to_i

      rescue StandardError => e
        # Decorate any JSON parsing exception or numeric exceptions when accessing and parsing the version field.
        raise "Unable to parse #{search_provider} response #{e}"
      end
      if (search_provider == 'elasticsearch') && !([2, 5, 6, 7].include?(version))
        raise "Unsupported Elasticsearch version of #{version}. There is currently support for the major versions of 2, 5, 6 and 7."
      end
      if (search_provider == 'opensearch') && !([1].include?(version))
        raise "Unsupported Opensearch version of #{version}. There is currently support for the major versions of 1."
      end
      version
    else
      # opscode-solr4 is enabled - this configuration setting should never be used in erlang.
      0
    end
  end

  def search_engine_index_definition
    if (search_provider == 'opensearch') && (search_engine_major_version == 1)
      os_1_index
    elsif search_provider == 'elastcisearch'
      if search_engine_major_version == 7
        es_7_index
      elsif search_engine_major_version == 6
        es_6_index
      elsif [2, 5].include?(search_engine_major_version)
        es_5_or_2_index
      end
    end
  end

  def os_1_index
    {
      'settings': {
        'analysis': {
          'analyzer': {
            'default': {
              'type': 'whitespace',
            },
          },
        },
        'number_of_shards':
          node['private_chef']['opensearch']['shard_count'],
        'number_of_replicas':
          node['private_chef']['opensearch']['replica_count'],
      },
      'mappings': {
        '_source': {
          'enabled': false,
        },
        'properties': {
          'X_CHEF_database_CHEF_X': {
            'type': 'keyword',
            'index': true,
            'norms': false,
          },
          'X_CHEF_type_CHEF_X': {
            'type': 'keyword',
            'index': true,
            'norms': false,
          },
          'X_CHEF_id_CHEF_X': {
            'type': 'keyword',
            'index': true,
            'norms': false,
          },
          'data_bag': {
            'type': 'keyword',
            'index': true,
            'norms': false,
          },
          'content': {
            'type': 'text',
            'index': true,
          },
        },
      },
    }
  end

  def es_7_index
    {
      'settings': {
        'analysis': {
          'analyzer': {
            'default': {
              'type': 'whitespace',
            },
          },
        },
        'number_of_shards':
          node['private_chef']['elasticsearch']['shard_count'],
        'number_of_replicas':
          node['private_chef']['elasticsearch']['replica_count'],
      },
      'mappings': {
        '_source': {
          'enabled': false,
        },
        'properties': {
          'X_CHEF_database_CHEF_X': {
            'type': 'keyword',
            'index': true,
            'norms': false,
          },
          'X_CHEF_type_CHEF_X': {
            'type': 'keyword',
            'index': true,
            'norms': false,
          },
          'X_CHEF_id_CHEF_X': {
            'type': 'keyword',
            'index': true,
            'norms': false,
          },
          'data_bag': {
            'type': 'keyword',
            'index': true,
            'norms': false,
          },
          'content': {
            'type': 'text',
            'index': true,
          },
        },
      },
    }
  end

  def es_6_index
    {
      'settings' => {
        'analysis' => {
          'analyzer' => {
            'default' => {
              'type' => 'whitespace',
            },
          },
        },
        'number_of_shards' =>
          node['private_chef']['elasticsearch']['shard_count'],
        'number_of_replicas' =>
          node['private_chef']['elasticsearch']['replica_count'],
      },
      'mappings' => {
        'object' => {
          '_source' => { 'enabled' => false },
          '_all' => { 'enabled' => false },
          'properties' => {
            'X_CHEF_database_CHEF_X' => {
              'type' => 'keyword',
              'norms' => {
                'enabled' => false,
              },
            },
            'X_CHEF_type_CHEF_X' => {
              'type' => 'keyword',
              'norms' => {
                'enabled' => false,
              },
            },
            'X_CHEF_id_CHEF_X' => {
              'type' => 'keyword',
              'norms' => {
                'enabled' => false,
              },
            },
            'data_bag' => {
              'type' => 'keyword',
              'norms' => {
                'enabled' => false,
              },
            },
            'content' => {
              'type' => 'text',
            },
          },
        },
      },
    }
  end

  def es_5_or_2_index
    {
      'settings' => {
        'analysis' => {
          'analyzer' => {
            'default' => {
              'type' => 'whitespace',
            },
          },
        },
        'number_of_shards' =>
          node['private_chef']['elasticsearch']['shard_count'],
        'number_of_replicas' =>
          node['private_chef']['elasticsearch']['replica_count'],
      },
      'mappings' => {
        'object' => {
          '_source' => { 'enabled' => false },
          '_all' => { 'enabled' => false },
          'properties' => {
            'X_CHEF_database_CHEF_X' => {
              'type' => 'string',
              'index' => 'not_analyzed',
              'norms' => {
                'enabled' => false,
              },
            },
            'X_CHEF_type_CHEF_X' => {
              'type' => 'string',
              'index' => 'not_analyzed',
              'norms' => {
                'enabled' => false,
              },
            },
            'X_CHEF_id_CHEF_X' => {
              'type' => 'string',
              'index' => 'not_analyzed',
              'norms' => {
                'enabled' => false,
              },
            },
            'data_bag' => {
              'type' => 'string',
              'index' => 'not_analyzed',
              'norms' => {
                'enabled' => false,
              },
            },
            'content' => { 'type' => 'string', 'index' => 'analyzed' },
          },
        },
      },
    }
  end

  def external_search_engine?
    if node['private_chef'][search_provider].key?('external')
      node['private_chef']['elasticsearch']['external']
    else
      false
    end
  end

  def search_engine_url
    if external_search_engine?
      node['private_chef'][search_provider]['external_url']
    elsif search_provider != 'opscode-solr4'
      "http://#{vip_for_uri(search_provider)}:#{node['private_chef'][search_provider]['port']}"
    end
  end

  # Returns scheme://host:port without any path
  # this method is not used anywhere, should we remove this?
  def solr_root
    url = URI.parse(search_engine_url)
    host = url.scheme + '://' + url.host
    if url.port
      host += ':' + url.port.to_s
    end
    host
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
    db_protocol = 'postgres'
    db_user     = node['private_chef']['opscode-erchef']['sql_connection_user'] || node['private_chef']['opscode-erchef']['sql_user']
    db_password = PrivateChef.credentials.get('opscode_erchef', 'sql_password')
    db_vip      = vip_for_uri('postgresql')
    db_name     = 'opscode_chef'

    "#{db_protocol}://#{db_user}:#{db_password}@#{db_vip}/#{db_name}"
  end

  def bifrost_db_connection_uri
    db_protocol = 'postgres'
    db_user     = node['private_chef']['oc_bifrost']['sql_connection_user'] || node['private_chef']['oc_bifrost']['sql_user']
    db_password = PrivateChef.credentials.get('oc_bifrost', 'sql_password')
    db_vip      = vip_for_uri('postgresql')
    db_name     = 'bifrost'

    "#{db_protocol}://#{db_user}:#{db_password}@#{db_vip}/#{db_name}"
  end

  # This file is touched once initial bootstrapping of the system is
  # done.
  def self.bootstrap_sentinel_file
    '/var/opt/opscode/bootstrapped'
  end

  # Use the presence of a sentinel file as an indicator for whether
  # the server has already had initial bootstrapping performed.
  #
  # @todo: Is there a more robust way to determine this, i.e., based
  #   on some functional aspect of the system?
  def self.has_been_bootstrapped?
    File.exist?(bootstrap_sentinel_file)
  end

  # Parse a config string as a memory value returning an integer in MB
  # units.  Supported inputs (not case sensitive) are B, K/Kb, M/Mb,
  # G/Gb. Uses integer division so values in B and Kb must exceed 1Mb.
  def self.parse_mem_to_mb(mem_str)
    if mem_str.is_a?(Integer)
      return mem_str
    end

    regex = /(\d+)([GgmMkKbB]{0,2})/
    m = regex.match(mem_str)
    raise 'bad arg' unless m

    raw_value = m[1].to_i
    units = m[2]
    value = case units
            when /^b$/i
              raw_value / (1024 * 1024)
            when /^kb?$/i
              raw_value / 1024
            when /^mb?$/i
              raw_value
            when '' # no units, assume Mb
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
      'undefined'
    end
  end

  def erl_atom_or_string(term)
    self.class.erl_atom_or_string(term)
  end

  def self.escape_characters_in_string(string)
    return '' unless string.is_a? String

    pattern = %r{(\'|\"|\.|\*|/|\-|\\)}
    string.gsub(pattern) { |match| '\\' + match }
  end

  def escape_characters_in_string(string)
    self.class.escape_characters_in_string(string)
  end

  def s3_url_caching(setting)
    case setting.to_s
    when 'off'
      'off'
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
    node['private_chef']['ldap'] && node['private_chef']['ldap']['enabled']
  end

  def platform_package_suffix
    case node['platform_family']
    when 'debian'
      'deb'
    when 'rhel', 'suse', 'amazon'
      'rpm'
    else
      # TODO: probably don't actually want to fail out?
      raise "I don't know how to install addons for platform family: #{node['platform_family']}"
    end
  end

  def remote_install_addons?
    # chef-solo and chef-client -z return different things :(
    node['private_chef']['addons']['path'].nil? || (node['private_chef']['addons']['path'] == {})
  end

  def self.chef_server_running_content(node)
    attrs = node['private_chef'].to_hash
    # To preserve compatibility with other add-ons and tools which use
    # the presence of an `ldap` key as an indicator that ldap is
    # enabled on Chef Infra Server, removed the ldap section if it's
    # disabled.
    unless attrs['ldap'] && attrs['ldap']['enabled']
      attrs['ldap'] = {}
    end

    # back-compat fixes for opscode-reporting opscode-reporting still
    # won't work with non-external Elasticsearch until reporting is
    # updated.
    attrs['opscode-solr4'] ||= {}
    attrs['opscode-solr4']['external'] = !!attrs['elasticsearch']['external']
    attrs['opscode-solr4']['external_url'] = attrs['elasticsearch']['external_url']

    content = {
      'private_chef' => attrs,
      'run_list' => node.run_list,
      'runit' => node['runit'].to_hash,
    }
    Chef::JSONCompat.to_json_pretty(content)
  end

  def postgresql_sslmodes
    # see: https://www.postgresql.org/docs/9.6/libpq-ssl.html
    # allowable choices come from the PGSSLMODE environment var for libpq.
    { 'disable' => false, 'require' => 'required' }
  end
end
