module ChefOpsDCC
  module Helpers
    def liveness_agent_recipe_url
      github_api = Chef::HTTP.new('https://api.github.com/')
      release = JSON.parse(github_api.get('/repos/chef/automate-liveness-agent/releases/latest'))
      release['assets'].find { |a| a['name'] == 'automate-liveness-recipe.rb' }['browser_download_url']
    end
  end
end

Chef::Node.send(:include, ChefOpsDCC::Helpers)
Chef::Recipe.send(:include, ChefOpsDCC::Helpers)
Chef::Resource.send(:include, ChefOpsDCC::Helpers)
