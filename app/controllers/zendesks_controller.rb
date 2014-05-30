class ZendesksController < ApplicationController
  include ZendesksHelper

  def show
    if zendesk_enabled?
      if signed_in?
        redirect_to zendesk_sso_url(current_user, params[:return_to])
      else
        # This is a little confusing, because we use a return_to session
        # variable internally for where you return to after signing in, but
        # the Zendesk SSO uses a return_to query parameter for where you should
        # go after you get sent back to Zendesk.
        session[:return_to] = zendesk_path(return_to: params[:return_to])
        redirect_to signin_path
      end
    else
      render template: 'errors/404', status: :not_found
      return
    end
  end
end
