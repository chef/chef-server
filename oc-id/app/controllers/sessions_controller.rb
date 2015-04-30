class SessionsController < ApplicationController
  rescue_from ActionController::InvalidAuthenticityToken, with: :destroy

  def new
    redirect_to oauth_authorized_applications_path if signed_in?
  end

  def create
    sign_in User.find(session_parameters[:uid])
    redirect_back_or oauth_authorized_applications_path
  end

  def retry
    flash.now[:error] = "Sorry, that username or password was incorrect. Please try again."
    render :new
  end

  def destroy
    sign_out
    redirect_to signin_path
  end

  private

    def credentials
      request.env['omniauth.auth']
    end

    def session_parameters
      { provider: credentials[:provider], uid: credentials[:uid] }
    end

end
