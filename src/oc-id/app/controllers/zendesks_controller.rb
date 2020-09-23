class ZendesksController < ApplicationController
  include SessionsHelper
  include ZendesksHelper

  before_action :not_found_if_zendesk_not_enabled

  def show
    if signed_in?
      redirect_to zendesk_sso_url(current_user, params[:return_to])
      return
    end

    # This is a little confusing, because we use a return_to session
    # variable internally for where you return to after signing in, but
    # the Zendesk SSO uses a return_to query parameter for where you should
    # go after you get sent back to Zendesk.
    session[:return_to] = zendesk_path(return_to: params[:return_to])
    redirect_to signin_path
  end

  # Zendesk wants to have a sign out URL that it does a GET to when the user
  # signs out of Zendesk. This is it.
  def signout
    sign_out
    redirect_to zendesk_path
  end

  private

  def not_found_if_zendesk_not_enabled
    render template: 'errors/404', status: :not_found unless zendesk_enabled?
  end
end
