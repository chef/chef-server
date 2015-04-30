class HomeController < ApplicationController

  def index
    redirect_to signed_in? ? oauth_authorized_applications_path : signin_path
  end

end
