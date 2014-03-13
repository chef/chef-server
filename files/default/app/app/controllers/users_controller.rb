class UsersController < ApplicationController
  doorkeeper_for :show
  respond_to :json

  def show
    owner = doorkeeper_token.resource_owner_id
    respond_with({ message: 'Access to this resource is restricted.' }, status: 401,) and return unless owner  == params[:id]

    @user = User.find(owner)
    respond_with @user
  end

end
