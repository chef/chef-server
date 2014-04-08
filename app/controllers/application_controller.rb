class ApplicationController < ActionController::Base
  include SessionsHelper

  protect_from_forgery with: :exception
  rescue_from ActionController::RoutingError, with: lambda { |exception| redirect_to error_path(404), status: 404 }
end
