module SessionsHelper

  def current_user=(user)
    @current_user = user
  end

  def current_user
    @current_user ||= User.find_by_username(session[:username])
  end

  def sign_in(user)
    session[:username] = user.username
    self.current_user = user
  end

  def signed_in?
    !current_user.nil?
  end

  def sign_out
    self.current_user = session[:username] = nil
  end

end
