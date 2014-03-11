class UsersController < ApplicationController
  doorkeeper_for :show
  respond_to :json

  def show
    @user = User.find_by_username(params[:id])
    respond_with @user
  end

end
