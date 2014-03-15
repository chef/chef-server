module Api
  module V1
    class UsersController < ApplicationController
      doorkeeper_for :all
      respond_to :json

      def show
        @user = User.find(params[:id])
        respond_with({ message: 'Resource not found' }, status: 404) and return if @user.nil?
        respond_with @user
      end

      def me
        @user = User.find(doorkeeper_token.resource_owner_id)
        respond_with @user
      end

    end
  end
end
