class ProfilesController < ApplicationController
  #
  # GET /id/profile
  #
  # Shows the user their profile page.
  #
  def show
    redirect_to signin_path unless signed_in?
    @user = current_user
  end

  #
  # PUT /id/profile
  #
  # Updates the current user's profile with the provided information.
  #
  def update
    @user = current_user
    updated_email = (permit_all_params.has_key? :email) && (permit_all_params[:email] != @user.email)

    # Note that if an email address is provided, don't update it right away.
    # Instead send a verification email and let that email link update the
    # email address.

    if @user.update_attributes(user_params)
      message = I18n.t('profile.update_successful')
      if updated_email
        EmailVerifyMailer.email_verify(@user, permit_all_params[:email]).deliver_now
        message << "\n" + I18n.t('profile.email_change_sent')
      end
      redirect_to profile_path, :notice => message
    else
      render 'show'
    end
  end

  #
  # PUT /id/profile/password
  #
  # Changes the user's password.
  #
  def change_password
    @user = current_user
    @user.update_password(params)

    if @user.errors.empty?
      redirect_to profile_path, :notice => I18n.t('profile.password_changed')
    else
      flash.now[:alert] = I18n.t('errors.passwords.problem')
      render 'show', :status => :forbidden
    end
  end

  #
  # GET /id/profile/change_email
  #
  # Changes the user's email.
  #
  # This should really be a PUT but we are using GET to make it easier to access
  # the end-point from an email without having to dive into javascript.
  # The initiating action for this is a POST/PUT to /id/profile that contains
  # an email change. The handler for that sends an email with a link to this
  # end-point to confirm the change.
  #
  # Important caveats: this method may be called when the user is signed into a
  # different account or when the user is not signed in at all. If we are unable
  # to redirect to a profile page because the user isn't signed in, we simply go
  # to the signin page.
  #
  def change_email
    profile_params =   decode_user_params
    @user = User.find(URI.encode_www_form_component(profile_params[:username])) if profile_params[:username].present?
    return goto_forbidden('errors.emails.invalid_username', name: profile_params[:username]) if @user.nil?
    return goto_forbidden('errors.emails.invalid_signature') unless valid_signature?

    @user.update_attributes('email' => profile_params[:email])
    return goto_forbidden('errors.emails.problem') unless @user.errors.empty?

    target_path = signed_in? ? profile_path : signin_path
    redirect_to target_path, :notice => I18n.t('profile.email_changed', name: profile_params[:username])
  end

  #
  # POST /id/profile/regen_key
  #
  # Resets the user's private key.
  #
  def regen_key
    @user = current_user
    @user.regen_private_key
    send_data @user.private_key, :type => "application/pem-keys", :disposition => "attachment", :filename => "#{@user.username}.pem"
  end

  private

  def goto_forbidden(message, *args)
    if signed_in?
      flash.now[:alert] = I18n.t(message, *args)
      render 'show', status: :forbidden
    else
      redirect_to signin_path, notice: I18n.t(message, *args)
    end
  end


  def valid_signature?
    return false unless [:signature, :user, :expires].all? { |p| params[p].present? }

    # Validate the signature against the user's current primary email address using
    # the new email address as a "payload" to verify that the email change request
    # link was not tampered with.
    Signature.new(
      @user.username,
      @user.email,
      params[:expires],
      Settings.secret_key_base,
      decode_user_params[:email],
    ).valid_for?(params[:signature])
  end
  
  def decode_user_params
    unsafe_input_encoded = params[:user]
    JSON.parse(Base64.urlsafe_decode64(unsafe_input_encoded),
                        symbolize_names: true,
                        create_additions: false
                      )
  end

  # When creating a new ActiveRecord model, only the permitted attributes are passed into the model
  def user_params
    params.require(:user).permit(:username, :first_name, :last_name, :middle_name, :public_key, :private_key, :display_name, :password)
  end

  def permit_all_params
    params.require(:user).permit(:username, :first_name, :last_name, :middle_name, :public_key, :private_key, :display_name, :password, :email)
  end
end
