
CREATE OR REPLACE FUNCTION test_enum_types()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$

BEGIN
  -- If you saw a failure after adding a new enum type, it's because you didn't
  -- add the type to this test. Don't forget to add a new test
  -- to verify the correct enum labels too.
  RETURN QUERY SELECT enums_are( ARRAY['password_hash_type',
                                       'org_state'],
                                 'expected enum types should exist');

  RETURN QUERY SELECT enum_has_labels( 'password_hash_type',
                                        ARRAY['SHA1',
                                              'SHA1-bcrypt',
                                              'bcrypt',
                                              'erlang-bcrypt-0.5.0',
                                              'sha1+bcrypt'],
                                        'password_hash_type should have known labels');

  RETURN QUERY SELECT enum_has_labels( 'org_state',
                                        ARRAY['holding',
                                              'ready',
                                              'started',
                                              'completed',
                                              'failed',
                                              'purge_started',
                                              'purge_successful'],
                                        'org_state should have known labels');
END;

$$;
