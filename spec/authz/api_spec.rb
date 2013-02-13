describe "pedant API" do
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
    #   (with_object)
    #   (with_container)
    # We then verify the permissions and memberships with
    #   directly_have_permission
    #     on_actor
    #     on_group
    #     (on_object)
    #     (on_container)
    #   be_a_direct_member_of

    context "for actors" do
      ['CREATE', 'READ', 'UPDATE', 'DELETE', 'GRANT'].each do |action|
        context "for #{action} ACE" do
          context "for single ACE on actor" do
            with_actors :hasselhoff, :shatner

            with_ace_on :shatner, action.downcase.to_sym, :actors => [:hasselhoff]

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
          
            with_acl_on :shatner, acl

            it "does not have permission" do
              :malkovich.should_not directly_have_permission(action.downcase.to_sym).
                on_actor(:shatner)
            end
          end

          context "an actor indirectly in the ACE" do
            with_actors :hasselhoff, :shatner
            with_group :hipsters, :actors => [:hasselhoff]

            with_ace_on :shatner, action.downcase.to_sym, :groups => [:hipsters]

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
            with_group :hipsters, :actors => [:hasselhoff]
            with_group :brogrammers, :groups => [:hipsters]

            with_ace_on :shatner, action.downcase.to_sym, :groups => [:brogrammers]

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
        with_acl_on :shatner, {
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
        with_acl_on :shatner, {
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
    end # for actors

    context "for groups" do
      ['CREATE', 'READ', 'UPDATE', 'DELETE', 'GRANT'].each do |action|
        context "for #{action} ACE" do
          context "for single ACE on actor" do
            with_actor :hasselhoff
            with_group :hipsters

            with_ace_on :hipsters, action.downcase.to_sym, :actors => [:hasselhoff]

            it "has permission" do
              :hasselhoff.should directly_have_permission(action.downcase.to_sym).
                on_group(:hipsters)
            end
          end

          context "an actor NOT in the ACE" do
            with_actor :malkovich
            with_group :hipsters

            # Give malkovich everything EXCEPT action
            acl = {
              :create => {:actors => [:malkovich], :groups => []},
              :read   => {:actors => [:malkovich], :groups => []},
              :update => {:actors => [:malkovich], :groups => []},
              :delete => {:actors => [:malkovich], :groups => []},
              :grant  => {:actors => [:malkovich], :groups => []}
            }
            acl[action.downcase.to_sym] = {:actors => [], :groups => []}
          
            with_acl_on :hipsters, acl

            it "does not have permission" do
              :malkovich.should_not directly_have_permission(action.downcase.to_sym).
                on_group(:hipsters)
            end
          end

          context "an actor indirectly in the ACE" do
            with_actor :hasselhoff
            with_group :hipsters, :actors => [:hasselhoff]
            with_group :commies

            with_ace_on :commies, action.downcase.to_sym, :groups => [:hipsters]

            it "has only indirect permission" do
              :hasselhoff.should_not directly_have_permission(action.downcase.to_sym).
                on_group(:commies)
              :hasselhoff.should be_a_direct_member_of(:hipsters)
              :hipsters.should directly_have_permission(action.downcase.to_sym).
                on_group(:commies)
            end
          end

          context "an actor doubly-indirectly in the ACE" do
            with_actor :hasselhoff
            with_group :hipsters, :actors => [:hasselhoff]
            with_group :brogrammers, :groups => [:hipsters]
            with_group :commies

            with_ace_on :commies, action.downcase.to_sym, :groups => [:brogrammers]

            it "has only doubly-indirect permission" do
              :hasselhoff.should_not directly_have_permission(action.downcase.to_sym).
                on_group(:commies)
              :hasselhoff.should be_a_direct_member_of(:hipsters)
              :hipsters.should be_a_direct_member_of(:brogrammers)
              :hipsters.should_not directly_have_permission(action.downcase.to_sym).
                on_group(:commies)
              :brogrammers.should directly_have_permission(action.downcase.to_sym).
                on_group(:commies)
            end
          end
        end
      end

      context "an actor with NO ACE" do
        with_actor :malkovich
        with_group :hipsters

        # Give malkovich no access at all
        with_acl_on :hipsters, {
          :create => {:actors => [], :groups => []},
          :read   => {:actors => [], :groups => []},
          :update => {:actors => [], :groups => []},
          :delete => {:actors => [], :groups => []},
          :grant  => {:actors => [], :groups => []}
        }

        it "has no permissions" do
          :malkovich.should_not directly_have_permission(:create).on_group(:hipsters) 
          :malkovich.should_not directly_have_permission(:read).on_group(:hipsters)
          :malkovich.should_not directly_have_permission(:update).on_group(:hipsters)
          :malkovich.should_not directly_have_permission(:delete).on_group(:hipsters)
          :malkovich.should_not directly_have_permission(:grant).on_group(:hipsters)
        end
      end

      context "an actor with full ACE" do
        with_actor :hasselhoff
        with_group :hipsters

        # Give hasselhoff full access
        with_acl_on :hipsters, {
          :create => {:actors => [:hasselhoff], :groups => []},
          :read   => {:actors => [:hasselhoff], :groups => []},
          :update => {:actors => [:hasselhoff], :groups => []},
          :delete => {:actors => [:hasselhoff], :groups => []},
          :grant  => {:actors => [:hasselhoff], :groups => []}
        }

        it "has all permissions" do
          :hasselhoff.should directly_have_permission(:create).on_group(:hipsters) 
          :hasselhoff.should directly_have_permission(:read).on_group(:hipsters)
          :hasselhoff.should directly_have_permission(:update).on_group(:hipsters)
          :hasselhoff.should directly_have_permission(:delete).on_group(:hipsters)
          :hasselhoff.should directly_have_permission(:grant).on_group(:hipsters)
        end
      end
    end # for groups
  end # sanity check
end
