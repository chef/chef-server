class ErrorsController < ApplicationController

  def show
    render template: "errors/#{params[:id]}"
  end

end