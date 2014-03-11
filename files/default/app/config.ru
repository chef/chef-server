# This file is used by Rack-based servers to start the application.

require ::File.expand_path('../config/environment',  __FILE__)

if defined?(Thin)
  require 'event_source_server'
  use EventSourceMiddleware
end

run OcId::Application
