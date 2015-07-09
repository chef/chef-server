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
  # POST /id/profile/regen_key
  #
  # Resets the user's private key.
  #
  def regen_key
    @user = current_user
    @user.regen_private_key
    send_data @user.private_key, :type => "application/pem-keys", :disposition => "attachment", :filename => "#{@user.username}.pem"
  end
end
