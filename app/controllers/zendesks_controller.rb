class ZendesksController < ApplicationController
  include ZendesksHelper

  def show
    render template: 'errors/404', status: :not_found unless zendesk_enabled?
  end
end
