OcId::Application.routes.draw do
  root 'home#index'
  get 'id', to: 'home#index'

  scope :id do
    use_doorkeeper

    get 'signin', to: 'sessions#new'
    delete 'signout', to: 'sessions#destroy'

    post '/auth/chef/callback', to: 'sessions#create'
    get '/auth/failure', to: 'sessions#retry'

    resources :sessions, only: [ :new, :create, :destroy ]
    resources :errors, only: :show

    namespace :v1 do
      resources :users, only: :show
      get 'me', to: 'users#me'
    end
  end
end
