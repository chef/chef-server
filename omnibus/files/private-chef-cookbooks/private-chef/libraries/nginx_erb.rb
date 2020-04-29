class NginxErb
  attr_reader :node

  def initialize(node)
    @node = node
  end

  # Sets up the variable used for the host header. If we're running on a
  # non-standard port (80 for http; 443 for https), we need to include the
  # port number in the host header, or redirects will not work because we will
  # lose the port number on the redirect.
  def host_header_var(proto)
    if proto == 'http'
      standard_port = 80
      port = node['private_chef']['nginx']['non_ssl_port'] || standard_port
    elsif proto == 'https'
      standard_port = 443
      port = node['private_chef']['nginx']['ssl_port'] || standard_port
    end
    "$host#{':$server_port' if port != standard_port}"
  end

  def default_orgname
    default_orgname = node['private_chef']['default_orgname']
    if default_orgname
      "\"#{default_orgname}\""
    else
      'false'
    end
  end

  def server_name
    if node['private_chef']['nginx']['use_implicit_hosts']
      "#{node['private_chef']['nginx']['server_name']} #{implicit_hosts}"
    else
      node['private_chef']['nginx']['server_name']
    end
  end

  def implicit_hosts
    @implicit_hosts ||= begin
                          hosts = [ 'localhost', '127.0.0.1' ]
                          hosts << '::1' if ipv6?

                          hosts << local_ip_addresses
                          if node['cloud']
                            hosts << node['cloud']['public_ips'] if node['cloud']['public_ips']
                            hosts << node['cloud']['private_ips'] if node['cloud']['private_ips']
                          end
                          hosts.flatten.uniq.join(' ')
                        end
  end

  def ipv6?
    node['private_chef']['nginx']['enable_ipv6']
  end

  def local_ip_addresses
    ret = []
    node['network']['interfaces'].each do |_name, iface|
      next unless iface['addresses'].respond_to?(:each)

      iface['addresses'].each do |addr, addr_info|
        if addr_info['family'] == 'inet'
          ret << addr
        elsif addr_info['family'] == 'inet6' && ipv6?
          ret << addr
        end
      end
    end
    ret
  end

  def listen_port(proto, options = {})
    listen_opts = ''
    listen_port = case proto
                  when 'http'
                    node['private_chef']['nginx']['non_ssl_port'].to_s || '80'
                  when 'https'
                    listen_opts << ' ssl'
                    node['private_chef']['nginx']['ssl_port'].to_s
                  else
                    proto.to_s
                  end
    listen_addr = ''
    if node['private_chef']['nginx']['enable_ipv6']
      listen_addr = '[::]:'
      if options[:ipv6_only]
        # Listen on IPv6 only, leaving IPv4 addresses alone.
        listen_opts << ' ipv6only=on'
      else
        # Listen on both IPv4, and IPv6.
        listen_opts << ' ipv6only=off'
      end
    end
    # Listen on IPv4 only.
    "listen #{listen_addr}#{listen_port}#{listen_opts};"
  end

  def access_log(proto)
    case proto
    when 'http'
      fname = "access-port-#{node['private_chef']['nginx']['non_ssl_port'] || 80}.log"
      "/var/log/opscode/nginx/#{fname}"
    when 'https'
      '/var/log/opscode/nginx/access.log'
    end
  end
end
