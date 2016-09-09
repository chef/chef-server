class PasswordResetsController < ApplicationController
  def new
  end

  def create
    if params[:username].blank?
      flash.now[:alert] = I18n.t("errors.password_resets.username_blank")
      render 'new', status: :unprocessable_entity
      return
    end

    begin
      user = User.find(escaped_username)

      if user.nil?
        user_not_found
      else
        PasswordResetMailer.password_reset(user).deliver_now
        flash.now[:notice] = I18n.t("errors.password_resets.completion")
        @status = :ok
      end
    rescue Net::HTTPServerException => e
      if e.response.code.to_i == 404
        user_not_found
      elsif e.response.code.to_i == 406
        flash.now[:alert] = I18n.t("errors.password_resets.invalid_username", :name => params[:username])
        @status = :forbidden
      else
        raise
      end
    end

    render 'new', status: @status
  end

  def show
    unless valid_signature?
      flash[:alert] = I18n.t("errors.password_resets.invalid_signature")
      redirect_to action: 'new'
    end
  end

  def update
    if params[:password].blank?
      flash.now[:alert] = I18n.t("errors.passwords.blank", :label => 'Password')
      render 'show', status: :forbidden
    elsif !valid_signature?
      flash.now[:alert] = I18n.t("errors.password_resets.invalid_signature")
      render 'show', status: :forbidden
    else
      begin
        # User should not be able to use a password reset link generated for an
        # old email address after the user has legitimately changed their
        # primary email. The email provided in the request can be trusted because
        # it's already verified when we examine the signature.
        user = User.find(escaped_username)
        if user.email != params[:email]
          flash.now[:alert] = I18n.t("errors.password_resets.invalid_signature")
          render 'show', status: :forbidden
        else
          user.update_attributes('password' => params[:password])
          session[:username] = user.username
          redirect_to signin_path
        end
      rescue Net::HTTPServerException => e
        if e.response.code.to_i == 404
          flash[:notice] = I18n.t("errors.password_resets.completion")
          redirect_to action: 'new'
        elsif e.response.code.to_i == 400
          flash.now[:alert] = error_from_json(e)['error']
          render 'show', status: e.response.code
        else
          raise
        end
      end
    end
  end

  private

  def user_not_found
    flash.now[:notice] = I18n.t("errors.password_resets.completion")
  end

  def escaped_username
    URI.escape(params[:username])
  end

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
