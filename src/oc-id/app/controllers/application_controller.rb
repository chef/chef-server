class ApplicationController < ActionController::Base
  include SessionsHelper

  protect_from_forgery with: :exception
  rescue_from ActionController::RoutingError, with: lambda { |exception| render template: 'errors/404', status: :not_found }

  protected

  def error_from_json(error)
    JSON.parse(error.response.body)
  rescue JSON::ParserError
    { 'error' => error.message }
  end
end
