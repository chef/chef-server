describe "pedant API" do
  let(:mattdamon) { "deadbeefdeadbeefdeadbeefdeadbeef" }

  context "sanity check" do
    # What we are testing:

    # Here we take all of the patterns used in the other tests and
    # verify that the permissions are correctly set up by the helper
    # functions.

    # We are explictly testing:
    #   with_ace_on_actor
    #   with_acl_on_actor
    # And indirectly:
    #   with_actor(s)
    #   with_group(s)
    #   with_members (for actors and groups)
    # We then verify the permissions and memberships with
    #   directly_have_permission
    #   be_a_direct_member_of

    ['CREATE', 'READ', 'UPDATE', 'DELETE', 'GRANT'].each do |action|
      context "for #{action} ACE" do
        context "for single ACE on actor" do
          with_actors :hasselhoff, :shatner

          with_ace_on_actor :shatner, action.downcase.to_sym, :actors => [:hasselhoff]

          it "has permission" do
            :hasselhoff.should directly_have_permission(action.downcase.to_sym).
              on_actor(:shatner)
          end
        end

        context "an actor NOT in the ACE" do
          with_actors :malkovich, :shatner

          # Give malkovich everything EXCEPT action
          acl = {
            :create => {:actors => [:malkovich], :groups => []},
            :read   => {:actors => [:malkovich], :groups => []},
            :update => {:actors => [:malkovich], :groups => []},
            :delete => {:actors => [:malkovich], :groups => []},
            :grant  => {:actors => [:malkovich], :groups => []}
          }
          acl[action.downcase.to_sym] = {:actors => [], :groups => []}
          
          with_acl_on_actor :shatner, acl

          it "does not have permission" do
            :malkovich.should_not directly_have_permission(action.downcase.to_sym).
              on_actor(:shatner)
          end
        end

        context "an actor indirectly in the ACE" do
          with_actors :hasselhoff, :shatner, :malkovich
          with_group :hipsters

          with_ace_on_actor :shatner, action.downcase.to_sym, :groups => [:hipsters]
          with_members :hipsters, :actors => [:hasselhoff]

          it "has only indirect permission" do
            :hasselhoff.should_not directly_have_permission(action.downcase.to_sym).
              on_actor(:shatner)
            :hasselhoff.should be_a_direct_member_of(:hipsters)
            :hipsters.should directly_have_permission(action.downcase.to_sym).
              on_actor(:shatner)
          end
        end

        context "an actor doubly-indirectly in the ACE" do
          with_actors :hasselhoff, :shatner
          with_groups :hipsters, :brogrammers

          with_ace_on_actor :shatner, action.downcase.to_sym, :groups => [:brogrammers]
          with_members :brogrammers, :groups => [:hipsters]
          with_members :hipsters, :actors => [:hasselhoff]

          it "has only doubly-indirect permission" do
            :hasselhoff.should_not directly_have_permission(action.downcase.to_sym).
              on_actor(:shatner)
            :hasselhoff.should be_a_direct_member_of(:hipsters)
            :hipsters.should be_a_direct_member_of(:brogrammers)
            :hipsters.should_not directly_have_permission(action.downcase.to_sym).
              on_actor(:shatner)
            :brogrammers.should directly_have_permission(action.downcase.to_sym).
              on_actor(:shatner)
          end
        end
      end
    end

    context "an actor with NO ACE" do
      with_actors :malkovich, :shatner

      # Give malkovich no access at all
      with_acl_on_actor :shatner, {
        :create => {:actors => [], :groups => []},
        :read   => {:actors => [], :groups => []},
        :update => {:actors => [], :groups => []},
        :delete => {:actors => [], :groups => []},
        :grant  => {:actors => [], :groups => []}
      }

      it "has no permissions" do
        :malkovich.should_not directly_have_permission(:create).on_actor(:shatner) 
        :malkovich.should_not directly_have_permission(:read).on_actor(:shatner)
        :malkovich.should_not directly_have_permission(:update).on_actor(:shatner)
        :malkovich.should_not directly_have_permission(:delete).on_actor(:shatner)
        :malkovich.should_not directly_have_permission(:grant).on_actor(:shatner)
      end
    end

    context "an actor with full ACE" do
      with_actors :hasselhoff, :shatner

      # Give hasselhoff full access
      with_acl_on_actor :shatner, {
        :create => {:actors => [:hasselhoff], :groups => []},
        :read   => {:actors => [:hasselhoff], :groups => []},
        :update => {:actors => [:hasselhoff], :groups => []},
        :delete => {:actors => [:hasselhoff], :groups => []},
        :grant  => {:actors => [:hasselhoff], :groups => []}
      }

      it "has all permissions" do
        :hasselhoff.should directly_have_permission(:create).on_actor(:shatner) 
        :hasselhoff.should directly_have_permission(:read).on_actor(:shatner)
        :hasselhoff.should directly_have_permission(:update).on_actor(:shatner)
        :hasselhoff.should directly_have_permission(:delete).on_actor(:shatner)
        :hasselhoff.should directly_have_permission(:grant).on_actor(:shatner)
      end
    end
  end # sanity check
end
