OcId::Application.routes.draw do
  root 'home#index'
  get 'id', to: 'home#index'

  scope :id do
    use_doorkeeper

    get 'signin', to: 'sessions#new'
    delete 'signout', to: 'sessions#destroy'

    post '/auth/chef/callback', to: 'sessions#create'
    get '/auth/failure', to: 'sessions#retry'

    resources :sessions, only: [:new, :create, :destroy]
    resource :password_reset, path: '/password-reset', except: [:destroy, :edit]

    resource :profile, only: [:edit, :update]

    resource :zendesk, only: [:show] do
      get 'signout', to: 'zendesks#signout'
    end

    namespace :v1 do
      resources :users, only: :show

      get 'me', to: 'users#me'
      get 'all_users', to: 'users#all_users'
      get 'me/organizations', to: 'users#organizations'
      get 'status', to: 'health#show'
    end
  end
end
