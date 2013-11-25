class NginxErb

  attr_reader :node

  def initialize(node)
    @node = node
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

  # Generate an nginx location directive, selecting opscode_chef or
  # opscode_erchef based on the node's dark_launch config.
  def chef_api(path, key=:erchef, alternative="opscode_webui", proto="http")
    # the following is totally gross and bizzare, but seems to
    # result in passably formatted nginx location stanza's when
    # rendered in our erb template for nginx config
    make_location(path, "opscode_erchef", alternative, proto)
  end

  def choose_account_upstream(key=:erchef)
    if (key == :account) || node['private_chef']['dark_launch'][key]
      "opscode_account"
    else
      "opscode_erchef"
    end
  end

  # Generate an nginx location directive, selecting opscode_account or
  # opscode_erchef based on the node's dark_launch config
  def account_api(path, key=:erchef, alternative="opscode_webui", proto="http")
    make_location(path, choose_account_upstream(key), alternative, proto)
  end

  ### NOTE: X-Ops-Darklaunch Header Setting - 2013/08/29 ###
  # This will clear X-Ops-Darklaunch headers set by the client
  # on their way to the chef server. The chef server should
  # not respond to darklaunch headers set by the client. Since
  # we're no longer using the darklaunch sub-request to set
  # these headers, we'll do it the simple way here.
  #
  # In the near future, the load balancer routing logic
  # will be re-written in lua and this block of code will
  # be removed. It is not expected that we will need to
  # darklaunch endpoints before that happens. If we do,
  # we should use the built-in darklaunch sub-request mechanism
  # that's in the nginx config.
  #
  def make_location(path, upstream, alternative, proto)
    <<EOS
location ~ "#{path}" {
    \tmore_set_input_headers 'X-Ops-Darklaunch:';
    \tset $my_upstream #{upstream};
    \tif ($http_x_ops_userid = "") {
    \t\tset $my_upstream #{alternative};
    \t}
    \tproxy_redirect #{proto}://$my_upstream /;
    \tproxy_pass #{proto}://$my_upstream;
    }
EOS
  end

  def select_upstream(upstream, alternative, proto)
    <<EOS
    \tset $my_upstream #{upstream};
    \tif ($http_x_ops_userid = "") {
    \t\tset $my_upstream #{alternative};
    \t}
    \tproxy_redirect #{proto}://$my_upstream /;
    \tproxy_pass #{proto}://$my_upstream;
EOS
  end
end
