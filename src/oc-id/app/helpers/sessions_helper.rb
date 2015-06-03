module SessionsHelper
  def current_user=(user)
    @current_user = user
  end

  def current_user
    @current_user ||= User.find(session[:username]) unless session[:username].nil?
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

  def redirect_back_or(default)
    redirect_to(session[:return_to] || default)
    session.delete(:return_to)
  end
end
