describe "Pedant API" do
  context "sanity check" do
    # What we are testing:

    # Here we take all of the patterns used in the other tests and
    # verify that the permissions are correctly set up by the helper
    # functions.

    # We are explictly testing:
    #   with_ace_on
    #   with_acl_on
    # And indirectly:
    #   with_actor(s)
    #   with_group
    #   with_entity
    # We then verify the permissions and memberships with
    #   directly_have_permission
    #     on
    #   be_a_direct_member_of

    context "for actors" do
      [:create, :read, :update, :delete, :grant].each do |action|
        context "for #{action.upcase} ACE" do
          context "for single ACE on actor" do
            with_actors :hasselhoff, :shatner

            with_ace_on :shatner, action, :to => :hasselhoff

            it "has permission" do
              :hasselhoff.should directly_have_permission(action).on(:actor, :shatner)
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
            acl[action] = {:actors => [], :groups => []}
          
            with_acl_on :shatner, acl

            it "does not have permission" do
              :malkovich.should_not directly_have_permission(action).on(:actor, :shatner)
            end
          end

          context "an actor indirectly in the ACE" do
            with_actors :hasselhoff, :shatner
            with_group :hipsters, :members => [:hasselhoff]

            with_ace_on :shatner, action, :to => :hipsters

            it "has only indirect permission" do
              :hasselhoff.should_not directly_have_permission(action).on(:actor, :shatner)
              :hasselhoff.should be_a_direct_member_of(:hipsters)
              :hipsters.should directly_have_permission(action).on(:actor, :shatner)
            end
          end

          context "an actor doubly-indirectly in the ACE" do
            with_actors :hasselhoff, :shatner
            with_group :hipsters, :members => [:hasselhoff]
            with_group :brogrammers, :members => [:hipsters]

            with_ace_on :shatner, action, :to => :brogrammers

            it "has only doubly-indirect permission" do
              :hasselhoff.should_not directly_have_permission(action).on(:actor, :shatner)
              :hasselhoff.should be_a_direct_member_of(:hipsters)
              :hipsters.should be_a_direct_member_of(:brogrammers)
              :hipsters.should_not directly_have_permission(action).on(:actor, :shatner)
              :brogrammers.should directly_have_permission(action).on(:actor, :shatner)
            end
          end
        end
      end

      context "an actor with NO ACE" do
        with_actors :malkovich, :shatner

        # Give malkovich no access at all
        with_acl_on :shatner, {
          :create => {:actors => [], :groups => []},
          :read   => {:actors => [], :groups => []},
          :update => {:actors => [], :groups => []},
          :delete => {:actors => [], :groups => []},
          :grant  => {:actors => [], :groups => []}
        }

        it "has no permissions" do
          :malkovich.should_not directly_have_permission(:create).on(:actor, :shatner) 
          :malkovich.should_not directly_have_permission(:read).on(:actor, :shatner)
          :malkovich.should_not directly_have_permission(:update).on(:actor, :shatner)
          :malkovich.should_not directly_have_permission(:delete).on(:actor, :shatner)
          :malkovich.should_not directly_have_permission(:grant).on(:actor, :shatner)
        end
      end

      context "an actor with full ACE" do
        with_actors :hasselhoff, :shatner

        # Give hasselhoff full access
        with_acl_on :shatner, {
          :create => {:actors => [:hasselhoff], :groups => []},
          :read   => {:actors => [:hasselhoff], :groups => []},
          :update => {:actors => [:hasselhoff], :groups => []},
          :delete => {:actors => [:hasselhoff], :groups => []},
          :grant  => {:actors => [:hasselhoff], :groups => []}
        }

        it "has all permissions" do
          :hasselhoff.should directly_have_permission(:create).on(:actor, :shatner) 
          :hasselhoff.should directly_have_permission(:read).on(:actor, :shatner)
          :hasselhoff.should directly_have_permission(:update).on(:actor, :shatner)
          :hasselhoff.should directly_have_permission(:delete).on(:actor, :shatner)
          :hasselhoff.should directly_have_permission(:grant).on(:actor, :shatner)
        end
      end
    end # for actors

    [:group, :container, :object].each do |type|
      context "for #{type} type" do
        [:create, :read, :update, :delete, :grant].each do |action|
          context "for #{action} ACE" do
            context "for single ACE on actor" do
              with_actors :hasselhoff, :shatner
              with_entity type, :gozer

              with_ace_on :gozer, action, :to => :hasselhoff

              it "has permission" do
                :hasselhoff.should directly_have_permission(action).on(type, :gozer)
              end

              it "other actor does not have permission" do
                :shatner.should_not directly_have_permission(action).on(type, :gozer)
              end
            end

            context "an actor NOT in the ACE" do
              with_actor :malkovich
              with_entity type, :gozer

              # Give malkovich everything EXCEPT action
              acl = {
                :create => {:actors => [:malkovich], :groups => []},
                :read   => {:actors => [:malkovich], :groups => []},
                :update => {:actors => [:malkovich], :groups => []},
                :delete => {:actors => [:malkovich], :groups => []},
                :grant  => {:actors => [:malkovich], :groups => []}
              }
              acl[action] = {:actors => [], :groups => []}
          
              with_acl_on :gozer, acl

              it "does not have permission" do
                :malkovich.should_not directly_have_permission(action).on(type, :gozer)
              end
            end

            context "an actor indirectly in the ACE" do
              with_actor :hasselhoff
              with_group :hipsters, :members => [:hasselhoff]
              with_entity type, :gozer

              with_ace_on :gozer, action, :to => :hipsters

              it "has only indirect permission" do
                :hasselhoff.should_not directly_have_permission(action).on(type, :gozer)
                :hasselhoff.should be_a_direct_member_of(:hipsters)
                :hipsters.should directly_have_permission(action).on(type, :gozer)
              end
            end

            context "an actor doubly-indirectly in the ACE" do
              with_actor :hasselhoff
              with_group :hipsters, :members => [:hasselhoff]
              with_group :brogrammers, :members => [:hipsters]
              with_entity type, :gozer

              with_ace_on :gozer, action, :to => :brogrammers

              it "has only doubly-indirect permission" do
                :hasselhoff.should_not directly_have_permission(action).on(type, :gozer)
                :hasselhoff.should be_a_direct_member_of(:hipsters)
                :hipsters.should be_a_direct_member_of(:brogrammers)
                :hipsters.should_not directly_have_permission(action).on(type, :gozer)
                :brogrammers.should directly_have_permission(action).on(type, :gozer)
              end
            end
          end
        end

        context "an actor with NO ACE" do
          with_actor :malkovich
          with_entity type, :gozer

          # Give malkovich no access at all
          with_acl_on :gozer, {
            :create => {:actors => [], :groups => []},
            :read   => {:actors => [], :groups => []},
            :update => {:actors => [], :groups => []},
            :delete => {:actors => [], :groups => []},
            :grant  => {:actors => [], :groups => []}
          }

          it "has no permissions" do
            :malkovich.should_not directly_have_permission(:create).on(type, :gozer) 
            :malkovich.should_not directly_have_permission(:read).on(type, :gozer)
            :malkovich.should_not directly_have_permission(:update).on(type, :gozer)
            :malkovich.should_not directly_have_permission(:delete).on(type, :gozer)
            :malkovich.should_not directly_have_permission(:grant).on(type, :gozer)
          end
        end

        context "an actor with full ACE" do
          with_actor :hasselhoff
          with_entity type, :gozer

          # Give hasselhoff full access
          with_acl_on :gozer, {
            :create => {:actors => [:hasselhoff], :groups => []},
            :read   => {:actors => [:hasselhoff], :groups => []},
            :update => {:actors => [:hasselhoff], :groups => []},
            :delete => {:actors => [:hasselhoff], :groups => []},
            :grant  => {:actors => [:hasselhoff], :groups => []}
          }

          it "has all permissions" do
            :hasselhoff.should directly_have_permission(:create).on(type, :gozer) 
            :hasselhoff.should directly_have_permission(:read).on(type, :gozer)
            :hasselhoff.should directly_have_permission(:update).on(type, :gozer)
            :hasselhoff.should directly_have_permission(:delete).on(type, :gozer)
            :hasselhoff.should directly_have_permission(:grant).on(type, :gozer)
          end
        end
      end
    end # for groups
  end # sanity check
end
