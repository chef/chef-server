class NginxErb

  attr_reader :node

  def initialize(node)
    @node = node
  end

  def listen_port(proto)
    listen_port = ""
    listen_port << "[::]:" if node['private_chef']['nginx']['enable_ipv6']
    listen_port << case proto
                   when "http"
                     node['private_chef']['nginx']['non_ssl_port'].to_s || "80"
                   when "https"
                     node['private_chef']['nginx']['ssl_port'].to_s
                   else
                     proto.to_s
                   end
    listen_port
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

  # Helper to extract static dark_launch configuration
  def xdl(key)
    node['private_chef']['dark_launch'][key] ? 1 : 0
  end

  def not_xdl(key)
    !node['private_chef']['dark_launch'][key] ? 1 : 0
  end

  def xdl_couchdb_headers
    xdl_couchdb_flag = not_xdl('sql_migration_phase_1')
    sql_xdl = %w(checksums clients cookbooks environments roles data).map { |key| "couchdb_#{key}=#{xdl_couchdb_flag}" }
    sql_xdl.join(';')
  end

  def rewrite_by_xdarklaunch_couchdb(options = {})
    # By default, use the endpoint in the location capture.
    # There is a possibility for refining this code better for clarity.
    dl_key = options[:xdl_couchdb_flag] ? "\"couchdb_#{options[:xdl_couchdb_flag]}\"" : "\"couchdb_\" .. ngx.var.endpoint"
    options[:sql_upstream]     ||= "http://opscode_erchef"
    options[:couchdb_upstream] ||= "http://opscode_chef"

<<EOS
        rewrite_by_lua '
            -- Get xdarklaunch and inject headers
            res = ngx.location.capture("/organizations/" .. ngx.var.org .. "/darklaunch",
                                       { ctx = ngx.ctx, share_all_vars = true })

            local dl_key = #{dl_key}

            -- we will default to routing to chef instead of erchef, so we are
            -- evaluating the falseness of the couchdb value to determine if
            -- we route to erchef:
            --   couchdb_$endpoint == true / nil will route to ruby chef
            --
            local sql_endpoint = ngx.ctx.dl_config[dl_key] == 0

            if sql_endpoint then
                ngx.var.chef_upstream = "#{options[:sql_upstream]}"
            else
                ngx.var.chef_upstream = "#{options[:couchdb_upstream]}"
            end
        ';
EOS
  end

  def xdl_chef_api(path, options = {})
out = <<EOS
location ~ "#{path}" {
\tset $chef_upstream "";
\tif ($http_x_ops_userid = "") {
\t\tset $chef_upstream "opscode_webui";
\t}

\tif ($http_x_ops_userid != "") {
\t\t#{rewrite_by_xdarklaunch_couchdb(options)}
\t}

\tproxy_redirect $chef_upstream /;
\tproxy_pass $chef_upstream;
}
EOS
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
