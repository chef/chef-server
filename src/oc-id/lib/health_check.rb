# frozen_string_literal: true

class HealthCheck
  include ChefResource

  OK = 'ok'.freeze
  NOT_OK = 'not ok'.freeze
  REACHABLE = 'reachable'.freeze
  TIMEOUT = 'timeout'.freeze
  UNREACHABLE = 'unreachable'.freeze
  ERRORING = 'erroring'.freeze
  AUTHERROR = 'authentication error'.freeze

  attr_reader :status, :erchef, :postgres

  def initialize
    @status = OK
    @erchef = { status: REACHABLE }
    @postgres = { status: REACHABLE }
  end

  #
  # Do a general health check.
  #
  def check
    erchef_health
    erchef_key
    postgres_health
    overall
  end

  #
  # Shortcut to see if all is well
  #
  def ok?
    @status == OK
  end

  private

  #
  # Try to ping erchef as an indicator of general health
  #
  def erchef_health
    erchef_health_metric do
      output = chef.get_rest '_status'

      @erchef[:status] = ERRORING if output['status'] != 'pong'
    end
  end

  #
  # Try to fetch /users/pivotal, which will only succeed if we have the correct
  # webui private key via the secrets file.
  #
  def erchef_key
    erchef_health_metric do
      chef.get_rest '/users/pivotal'
    end
  end

  #
  # Check the number of active Postgres connections as an indicator of
  # general health
  #
  def postgres_health
    ActiveRecord::Base.connection.query(
      'SELECT count(*) FROM pg_stat_activity'
    ).flatten.first.to_i.tap do |connections|
      @postgres[:connections] = connections
    end
  rescue ActiveRecord::ConnectionTimeoutError
    @postgres[:status] = TIMEOUT
  rescue PG::ConnectionBad
    @postgres[:status] = UNREACHABLE
  end

  #
  # What is the overall system status
  #
  def overall
    return unless @postgres[:status] != REACHABLE || @erchef[:status] != REACHABLE

    @status = NOT_OK
  end

  def erchef_health_metric
    yield
  rescue Errno::ETIMEDOUT
    @erchef[:status] = TIMEOUT
  rescue Net::HTTPServerException => e
    @erchef[:status] = if e.message =~ /401/
                         AUTHERROR
                       else
                         ERRORING
                       end
  end
end
