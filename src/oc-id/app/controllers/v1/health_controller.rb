require "health_check"

module V1
  class HealthController < ApplicationController
    respond_to :json

    #
    # The main idea here is we want a way to check the status of oc-id and know
    # at a glance some key metrics which will be easy to monitor and tell us if
    # things are in a generally good or bad place.
    #
    def show
      @health = HealthCheck.new
      @health.check

      status = @health.ok? ? :ok : :service_unavailable

      respond_with @health, status: status
    end
  end
end
