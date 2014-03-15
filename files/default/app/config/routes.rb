OcId::Application.routes.draw do
  use_doorkeeper
  root 'home#index'

  get 'signin', to: 'sessions#new'
  delete 'signout', to: 'sessions#destroy'

  post '/auth/chef/callback', to: 'sessions#create'
  get '/auth/failure', to: 'sessions#retry'

  resources :sessions, only: [ :new, :create, :destroy ]

  namespace :api do
    namespace :v1 do
      resources :users, only: :show
      get 'me', to: 'users#me'
    end
  end
end
