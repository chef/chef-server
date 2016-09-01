class EmailVerifyMailer < ActionMailer::Base
  # Input:
  #   user: chef object returned by server
  # 
  # Sends an email to the given user to ascertain that said user controls the
  # email address requested. Note that the signature embeds the new email
  # address here. This is because we want to actually verify that the user
  # owns the new email address. We don't just want to forward the user a
  # "here's a way to just put in whatever email you want in this POST
  # request" token.
  def email_verify(user, email)
    @user = user
    @email = email
    @expires = 1.day.from_now.to_i
    @signature = Signature.new(
      @user.username,
      email,
      @expires,
      Settings.secret_key_base
    )
    mail from: Settings.email_from_address,
         subject: "Verify Your Chef Email",
         to: email
  end
end
