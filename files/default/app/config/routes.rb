OcId::Application.routes.draw do
  use_doorkeeper
  root 'home#index'

  match 'signin', to: 'sessions#new', via: :get
  match 'signout', to: 'sessions#destroy', via: :delete

  post '/auth/chef/callback', to: 'sessions#create'
  get '/auth/failure', to: 'sessions#retry'

  resources :sessions, only: [ :new, :create, :destroy ]
  resources :users, only: :show
end
