class NginxErb

  attr_reader :node

  def initialize(node)
    @node = node
  end

  def listen_port(proto)
    case proto
    when "http"
      node['private_chef']['nginx']['non_ssl_port'] || 80
    when "https"
      node['private_chef']['nginx']['ssl_port']
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

  def choose_upstream(key=:erchef)
    if (key == :ruby) || node['private_chef']['dark_launch'][key]
      "opscode_chef"
    else
      "opscode_erchef"
    end
  end

  # Generate an nginx location directive, selecting opscode_chef or
  # opscode_erchef based on the node's dark_launch config.
  def chef_api(path, key=:erchef, alternative="opscode_webui", proto="http")
    # the following is totally gross and bizzare, but seems to
    # result in passably formatted nginx location stanza's when
    # rendered in our erb template for nginx config
    make_location(path, choose_upstream(key), alternative, proto)
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

  def make_location(path, upstream, alternative, proto)
    <<EOS
location ~ "#{path}" {
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
