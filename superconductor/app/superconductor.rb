class Superconductor < Sinatra::Base

  class Config < Struct.new(:nginx_config)
  end

  def self.config
    @config ||= Config.new
  end

  def config
    self.class.config
  end

  configure(:production) do
    require File.expand_path("../../config/production.rb", __FILE__)
  end

  configure(:development) do
    require File.expand_path("../../config/development.rb", __FILE__)
  end

  NGINX_TEMPLATE = IO.read(File.expand_path('../hosted-chef-nginx.conf', __FILE__))
  COMPILED_TEMPLATE = Erubis::Eruby.new(NGINX_TEMPLATE)

  JSON_OK = Yajl::Encoder.encode({:ok => 'ok'}).freeze

  get "/status" do
    JSON_OK
  end

  get "/ping" do
    "PONG"
  end

  post "/debug" do
    req_data = request.body.read
    unless couchdb_orgs = Yajl::Parser.parse(req_data, :symbolize_keys => true)[:couchdb_orgs]
      halt(400, %Q|cool data bro. next time, try {"couchdb_orgs": ["org1", "org2", ...]}|)
    end
    new_nginx_config = nginx_config_for(couchdb_orgs)
    write_nginx_config(new_nginx_config)
    hup_nginx
    new_nginx_config
  end

  post "/configure", :provides => ['application/json'] do
    req_data = request.body.read
    unless couchdb_orgs = Yajl::Parser.parse(req_data, :symbolize_keys => true)[:couchdb_orgs]
      halt(400, %Q|cool data bro. next time, try {"couchdb_orgs": ["org1", "org2", ...]}|)
    end
    new_nginx_config = nginx_config_for(couchdb_orgs)
    write_nginx_config(new_nginx_config)
    hup_nginx
    JSON_OK
  end

  def write_nginx_config(nginx_config)
    File.open(config.nginx_config, IO::RDWR | IO::CREAT |IO::TRUNC ) do |f|
      f.flock(File::LOCK_EX | File::LOCK_NB)
      f.print(nginx_config)
    end
  end

  def hup_nginx
    pid = IO.read("/var/run/nginx.pid").to_i
    if pid == 0
      halt(500, "error reading nginx pid")
    end
    Process.kill(:HUP, pid)
  end

  def nginx_config_for(orgs_in_couch)
    template_context = { :couchdb_orgs => orgs_in_couch}
    COMPILED_TEMPLATE.evaluate(template_context)
  end

end
