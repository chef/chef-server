class SessionsController < ApplicationController

  def create
    sign_in User.find(session_parameters[:uid])
    redirect_to oauth_authorized_applications_path
  end 

  def retry
    flash.now[:danger] = 'Oops! Try again.'
    render :new
  end

  def destroy
    sign_out
    redirect_to root_path
  end

  private

    def credentials
      request.env['omniauth.auth']
    end

    def session_parameters
      { provider: credentials[:provider], uid: credentials[:uid] }
    end

end
