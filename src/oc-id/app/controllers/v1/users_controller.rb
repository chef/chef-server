module V1
  class UsersController < ApplicationController
    doorkeeper_for :all, except: [ :show, :all_users ]

    respond_to :json

    def show
      app = Doorkeeper::Application.find_by_uid(params[:app_id])
      head :unauthorized and return unless app

      @user = User.find(params[:id])
      head :not_found and return unless @user

      respond_with @user.public
    end

    def me
      @user = User.find(doorkeeper_token.resource_owner_id)

      respond_with @user
    end

    def organizations
      @user = User.find(doorkeeper_token.resource_owner_id)

      respond_with @user.organizations
    end

    def all_users
      app = Doorkeeper::Application.find_by_uid(params[:app_id])
      head :unauthorized and return unless app

      @users = User.all

      respond_with @users
    end
  end
end
