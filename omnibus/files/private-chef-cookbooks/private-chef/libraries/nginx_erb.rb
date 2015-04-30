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
      "false"
    end
  end

  def listen_port(proto, options = {})
    listen_port = ""
    listen_port << case proto
                   when "http"
                     node['private_chef']['nginx']['non_ssl_port'].to_s || "80"
                   when "https"
                     node['private_chef']['nginx']['ssl_port'].to_s
                   else
                     proto.to_s
                   end

    if node['private_chef']['nginx']['enable_ipv6']
      # In some cases, we're serving as a front-end for a service that's already
      # listening on the same port in ipv4 - this prevents a conflict in that situation.
      if options[:ipv6_only]
        "listen [::]:#{listen_port} ipv6only=on;"
      else
        # Listen to the same port on both v6 and v4
        "listen [::]:#{listen_port} ipv6only=off;"
      end
    else
      # default behavior to listen only on v4
      "listen #{listen_port};"
    end
  end

  def access_log(proto)
    case proto
    when "http"
      fname = "access-port-#{node['private_chef']['nginx']['non_ssl_port'] || 80}.log"
      "/var/log/opscode/nginx/#{fname}"
    when "https"
      "/var/log/opscode/nginx/access.log"
    end
  end
end
