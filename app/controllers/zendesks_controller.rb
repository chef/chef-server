class ZendesksController < ApplicationController
  def show
    render template: 'errors/404', status: :not_found
  end
end
