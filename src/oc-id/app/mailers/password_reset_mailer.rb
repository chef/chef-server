class PasswordResetMailer < ActionMailer::Base
  # Input:
  #   user: chef object returned by server
  def password_reset(user)
    @user = user
    @expires = 1.day.from_now.to_i
    @signature = Signature.new(
      @user.username,
      @user.email,
      @expires,
      Settings.secret_key_base
    )
    mail from: Settings.email_from_address,
         subject: "Reset Your Chef Password",
         to: @user.email
  end
end
