class SessionsController < ApplicationController

  def new
  end

  def create
    user = User.authenticate(params[:session][:username], params[:session][:password])
    if user
      sign_in user
      redirect_to oauth_authorized_applications_path
    else
      flash.now[:danger] = 'Oops! Try again.'
      render :new
    end
  end 

  def destroy
    sign_out
    redirect_to root_path
  end

end
