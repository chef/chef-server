class Chef
  module Web
    module Core
      module URLHelpers

        def chef_domain
          ENV['CHEF_DOMAIN'] || 'chef.io'
        end

        def chef_account_management_url
          ENV['CHEF_ACCOUNT_MANAGEMENT_URL'] || "#{chef_www_url}/account"
        end

        def chef_server_url
          ENV['CHEF_SERVER_URL'] || "https://api.#{chef_domain}"
        end

        def chef_www_url(extra = nil)
          url = ENV['CHEF_WWW_URL'] || "https://www.#{chef_domain}"
          extra_dispatch(url, extra)
        end

        def chef_blog_url(extra = nil)
          url = ENV['CHEF_BLOG_URL'] || "#{chef_www_url}/blog"
          extra_dispatch(url, extra)
        end

        def chef_docs_url(extra = nil)
          url = ENV['CHEF_DOCS_URL'] || "https://docs.#{chef_domain}"
          extra_dispatch(url, extra)
        end

        def chef_downloads_url(extra = nil)
          url = ENV['CHEF_DOWNLOADS_URL'] || "https://downloads.#{chef_domain}"
          extra_dispatch(url, extra)
        end

        def chef_identity_url
          ENV['CHEF_IDENTITY_URL'] || "https://id.#{chef_domain}/id"
        end

        def chef_manage_url
          ENV['CHEF_MANAGE_URL'] || "https://manage.#{chef_domain}"
        end

        def chef_sign_up_url
          ENV['CHEF_SIGN_UP_URL'] || "#{chef_manage_url}/signup"
        end

        def chef_status_url
          ENV['CHEF_STATUS_URL'] || "http://status.#{chef_domain}"
        end

        def learn_chef_url(extra = nil)
          url = ENV['LEARN_CHEF_URL'] || "https://learn.#{chef_domain}"
          extra_dispatch(url, extra)
        end

        def supermarket_url
          ENV['SUPERMARKET_URL'] || "https://supermarket.#{chef_domain}"
        end

        def chef_supermarket_url
          supermarket_url
        end

        def chef_facebook_url
          'https://www.facebook.com/getchefdotcom'
        end

        def chef_twitter_url
          'https://twitter.com/chef'
        end

        def chef_youtube_url
          'https://www.youtube.com/user/getchef'
        end

        def chef_linkedin_url
          'https://www.linkedin.com/groups/Chef-Users-Group-3751378'
        end

        private

        def extra_dispatch(url, extra = nil)
          if extra.nil?
            url
          else
            "#{url}/#{extra}"
          end
        end
      end
    end
  end
end
