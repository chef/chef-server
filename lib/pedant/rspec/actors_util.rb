module Pedant
  module RSpec
    module ActorsUtil
      # rspec dsl is mixed into every module

      shared_context 'create an actor as' do
        # This whole '@actor_id' thing is a bit hacky, but I don't know
        # of another way to pass information from inside an example back
        # out.  This was required because, unlike with the Chef API, we
        # cannot specify the Authz ID in advance.
        #
        # Alternatively, we could just not bother with cleaning up after
        # ourselves.  This would mean, however, that we couldn't share
        # actors, groups, etc. between examples, lest the tests begin to
        # "poison" on another.  Given the DSL we have going, that
        # probably wouldn't be too onerous, but would be a source of
        # potentially nasty testing bugs if we didn't adhere to it 100%.
        # It's also surprising behavior to people with RSpec experience.
        #
        # If you want the request to use the normal headers, just supply
        # a requestor.  If you want to completely replace all the
        # headers with ones you specify, use the +:headers+ option key.
        # If you just want to remove or modify a handful of headers, and
        # use the default values for the rest, supply them with a
        # +:merge_headers+ option key.  +:headers+ has precedence over
        # +:merge_headers+
        #
        # See +authz_request+ in +lib/pedant/rspec/common.rb+ for more.
        def self.creates_an_actor_as(requestor, header_modifications={})
          after :each do
            if @actor_id
              delete("/actors/#{@actor_id}", :superuser)
            end
          end

          it "creates an actor" do
            response = if header_modifications[:headers]
                         post("/actors", requestor,
                              :headers => header_modifications[:headers])
                       elsif header_modifications[:merge_headers]
                         post("/actors", requestor,
                              :merge_headers => header_modifications[:merge_headers])
                       else
                         post("/actors", requestor) # Use default "normal" headers
                       end

            @actor_id = parse(response)["id"]
            response.should be_create_response_for :actor
          end
        end
      end
    end
  end
end
