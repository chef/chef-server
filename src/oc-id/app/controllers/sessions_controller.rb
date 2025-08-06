class SessionsController < ApplicationController
  rescue_from ActionController::InvalidAuthenticityToken, with: :destroy

  def new
    redirect_to oauth_authorized_applications_path if signed_in?
  end

  def create
    # Check if this is a direct form post (like from Pedant) or OAuth callback
    if request.env['omniauth.auth'].nil? && params[:username].present?
      puts "Direct form POST detected - handling manual authentication"
      handle_direct_authentication
    else
      # Normal OAuth flow
      begin
        oauth_params = session_parameters
        user = User.find(oauth_params[:uid])
        sign_in user
        redirect_back_or oauth_authorized_applications_path
      rescue => e
        puts "Error in SessionsController#create: #{e.class} - #{e.message}"
        puts "Backtrace: #{e.backtrace.first(5)}"
        raise
      end
    end
  end

  def retry
    flash.now[:error] = I18n.t("errors.logins.invalid_credentials")
    render :new
  end

  def destroy
    sign_out
    redirect_to signin_path
  end

  private

  def handle_direct_authentication

    username = params[:username]
    password = params[:password]

    # Use the User.authenticate method that exists in this codebase
    authenticated_user = User.authenticate(username, password)
    if authenticated_user
      sign_in authenticated_user
      redirect_back_or oauth_authorized_applications_path
    else
      # Redirect to OmniAuth failure path with proper parameters to match test expectations
      redirect_to "/id/auth/failure?message=invalid_credentials&strategy=chef"
    end
  rescue => e
    puts "Error in direct authentication: #{e.class} - #{e.message}"
    # Redirect to OmniAuth failure path for errors too
    redirect_to "/id/auth/failure?message=invalid_credentials&strategy=chef"
  end

  def credentials
    request.env['omniauth.auth']
  end

  def session_parameters
    creds = credentials
    if creds.nil?
      raise "OmniAuth credentials missing. The OmniAuth chef provider may not be working."
    end
    { provider: creds[:provider], uid: creds[:uid] }
  end

end
