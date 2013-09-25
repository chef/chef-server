define :component_runit_service, :log_directory => nil, :svlogd_size => nil, :svlogd_num => nil, :ha => nil, :control => nil, :enable => nil do

  component = params[:name]
  log_directory = params[:log_directory] || node['private_chef'][component]['log_directory']
  enable = if params[:enable].nil?
             node['private_chef'][component]['enable']
           else
             params[:enable]
           end

  # Currently, only the opscode-expander-reindexer service declaration
  # uses this :ha flag; all others rely on the node attribute
  high_availability = if params[:ha].nil?
                        node['private_chef'][component]['ha']
                      else
                        params[:ha]
                      end

  runit_service component do
    action :nothing
    retries 20
    control params[:control] if params[:control]
    options(
      :log_directory => log_directory
    )
  end

  template "#{log_directory}/config" do
    source "config.svlogd"
    mode "0644"
    owner "root"
    group "root"
    variables(
      :svlogd_size => ( params[:svlogd_size] || node['private_chef'][component]['log_rotation']['file_maxbytes']),
      :svlogd_num  => ( params[:svlogd_num] || node['private_chef'][component]['log_rotation']['num_to_keep'])
    )
  end

  if high_availability
    log "bring runit_service[#{component}] down" do
      notifies :down, "runit_service[#{component}]", :immediately
    end
  end

  if enable
    log "enable runit_service[#{component}]" do
      notifies :enable, "runit_service[#{component}]", :immediately
    end
  end

end
