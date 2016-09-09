class ProfilesController < ApplicationController
  #
  # GET /id/profile
  #
  # Shows the user their profile page.
  #
  def show
    @user = current_user
  end

  #
  # PUT /id/profile
  #
  # Updates the current user's profile with the provided information.
  #
  def update
    @user = current_user

    # Note that if an email address is provided, don't update it right away.
    # Instead send a confirmation email and let that email link update the
    # email address.
    if params[:user].key?(:email) && @user.email != params[:user][:email]
      EmailVerifyMailer.email_verify(@user, params[:user][:email]).deliver_now
      params[:user].delete(:email)
    end

    if @user.update_attributes(params[:user])
      redirect_to profile_path, :notice => I18n.t('profile.update_successful')
    else
      render "show"
    end
  end

  #
  # PUT /id/profile/change_password
  #
  # Changes the user's password.
  #
  def change_password
    @user = current_user
    @user.update_password(params)

    if @user.errors.empty?
      redirect_to profile_path, :notice => I18n.t('profile.password_changed')
    else
      flash.now[:alert] = I18n.t("errors.passwords.problem")
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
  def change_email
    @user = current_user

    unless valid_signature?
      flash.now[:alert] = I18n.t("errors.emails.invalid_signature")
      return render 'show', :status => :forbidden
    end

    @user.update_attributes('email' => params[:email])

    if @user.errors.empty?
      redirect_to profile_path, :notice => I18n.t('profile.email_changed')
    else
      flash.now[:alert] = I18n.t("errors.emails.problem")
      render 'show', :status => :forbidden
    end
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

  def valid_signature?
    [:signature, :username, :email, :expires].all? { |p| params[p].present? } &&
    Signature.new(
      params[:username],
      params[:email],
      params[:expires],
      Settings.secret_key_base
    ).valid_for?(params[:signature])
  end
end
