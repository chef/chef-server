%w{webui_priv.pem actions-source.json}.each do |secrets_file|
  file ::File.join("/etc/opscode-analytics/", secrets_file) do
    action :delete
  end
end
