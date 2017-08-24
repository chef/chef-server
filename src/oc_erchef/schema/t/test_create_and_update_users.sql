CREATE OR REPLACE FUNCTION test_create_and_update_users()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

-- v0 add_user tests --
RETURN QUERY SELECT is(add_user('11111111111111111111111111111111',
                                '22222222222222222222222222222222',
                                'test_user_v0',
                                'test_user_v0@chef.io',
                                '--BEGIN PUBLIC KEY--END PUBLIC KEY--',
                                0,
                                'hahed_password',
                                'salt',
                                'bcrypt',
                                'dddddddddddddddddddddddddddddddd',
                                '2017-09-06 19:18:35',
                                '2017-09-06 19:18:35',
                                null,
                                false,
                                'serialized_object',
                                false),
                       1,
                       'add_user test_user_v0');

PREPARE "add id v0" AS SELECT public_key from users
                         WHERE id='11111111111111111111111111111111';
RETURN QUERY SELECT results_eq(
    '"add id v0"',
    ARRAY['this_is_not_a_key'],
    'do not insert public_key into the users table for v0'
);

PREPARE "add key v0" AS SELECT public_key FROM keys
                          WHERE id='11111111111111111111111111111111';
RETURN QUERY SELECT results_eq(
    '"add key v0"',
    ARRAY['--BEGIN PUBLIC KEY--END PUBLIC KEY--'],
    'insert user given public_key into the keys table for v0'
);

-- v1 add_user tests --

RETURN QUERY SELECT is(add_user('11111111111111111111111111111112',
                                '22222222222222222222222222222223',
                                'test_user_v1',
                                'test_user_v1@chef.io',
                                'this_is_not_a_key',
                                0,
                                'hashed_password',
                                'salt',
                                'bcrypt',
                                'dddddddddddddddddddddddddddddddd',
                                '2017-09-06 19:18:35',
                                '2017-09-06 19:18:35',
                                null,
                                false,
                                'serialized_object',
                                false),
                       1,
                       'add_user test_user_v1');

PREPARE "add id v1" AS SELECT public_key from users
                         WHERE id='11111111111111111111111111111112';
RETURN QUERY SELECT results_eq(
    '"add id v1"',
    ARRAY['this_is_not_a_key'],
    'do not insert public_key into the users table for v1'
);

PREPARE "add key v1" AS SELECT public_key FROM keys
                          WHERE id='11111111111111111111111111111112';
RETURN QUERY SELECT is_empty(
    '"add key v1"',
    'do not insert public_key into the keys table for v1;'
);

-- v0 update_user tests --

PREPARE "before update email v0" AS SELECT email from users
                                      WHERE id='11111111111111111111111111111111';
RETURN QUERY SELECT results_eq(
    '"before update email v0"',
    ARRAY['test_user_v0@chef.io'],
    'before update email address for v0'
);

RETURN QUERY SELECT is(update_user(0,
                                   '--BEGIN PUBLIC KEY--END PUBLIC KEY--',
                                   'hashed_password',
                                   'salt',
                                   'bcrypt',
                                   'changed_something_in_my_name_v0',
                                   null,
                                   false,
                                   'changed_email_address_v0@chef.io',
                                   'test_user_v0',
                                   'dddddddddddddddddddddddddddddddd',
                                   '2017-09-06 19:20:35',
                                   false,
                                   '11111111111111111111111111111111'),
                       1,
                       'update_user v0');

PREPARE "update id v0" AS SELECT public_key from users
                            WHERE id='11111111111111111111111111111111';
RETURN QUERY SELECT results_eq(
    '"update id v0"',
    ARRAY['this_is_not_a_key'],
    'unchanged public_key in users table after update for v0'
);

PREPARE "after update email v0" AS SELECT email from users
                                     WHERE id='11111111111111111111111111111111';
RETURN QUERY SELECT results_eq(
    '"after update email v0"',
    ARRAY['changed_email_address_v0@chef.io'],
    'email address updates after update_user function for v0'
);

PREPARE "update key v0" AS SELECT public_key FROM keys
                             WHERE id='11111111111111111111111111111111';
RETURN QUERY SELECT results_eq(
    '"update key v0"',
    ARRAY['--BEGIN PUBLIC KEY--END PUBLIC KEY--'],
    'unchanged public_key in keys table for v0'
);

-- v1 update_user tests --

PREPARE "before update email v1" AS SELECT email from users
                                      WHERE id='11111111111111111111111111111111';
RETURN QUERY SELECT results_eq(
    '"before update email v1"',
    ARRAY['changed_email_address_v0@chef.io'],
    'confirm email address before update for v1'
);

PREPARE "before update key v1" AS SELECT public_key FROM keys
                             WHERE id='11111111111111111111111111111111';
RETURN QUERY SELECT results_eq(
    '"before update key v1"',
    ARRAY['--BEGIN PUBLIC KEY--END PUBLIC KEY--'],
    'confirm public_key in keys table before update for v1'
);

RETURN QUERY SELECT is(update_user(0,
                                   'this_is_not_a_key',
                                   'bbbb',
                                   'cccc',
                                   'bcrypt',
                                   'changed_something_else_in_my_name',
                                   null,
                                   false,
                                   'changed_email_address_v1@chef.io',
                                   'test_user_v0',
                                   'dddddddddddddddddddddddddddddddd',
                                   '2017-09-06 19:20:35',
                                   false,
                                   '11111111111111111111111111111111'),
                       1,
                       'run the update_user function v1');

PREPARE "update id v1" AS SELECT public_key from users
                            WHERE id='11111111111111111111111111111111';
RETURN QUERY SELECT results_eq(
    '"update id v1"',
    ARRAY['this_is_not_a_key'],
    'unchanged public_key in users table after update for v1'
);

PREPARE "after update email v1" AS SELECT email from users
                                     WHERE id='11111111111111111111111111111111';
RETURN QUERY SELECT results_eq(
    '"after update email v1"',
    ARRAY['changed_email_address_v1@chef.io'],
    'email address rightly updated after update_user for v1'
);

PREPARE "update key v1" AS SELECT public_key FROM keys
                             WHERE id='11111111111111111111111111111111';
RETURN QUERY SELECT results_eq(
    '"update key v1"',
    ARRAY['--BEGIN PUBLIC KEY--END PUBLIC KEY--'],
    'unchanged public_key in keys table after update for v1'
);

-- update_user deletes the key if null is set in v0 --

RETURN QUERY SELECT is(update_user(0,
                                   null,
                                   'bbbb',
                                   'cccc',
                                   'bcrypt',
                                   'changed_something_again_in_my_name',
                                   null,
                                   false,
                                   'changed_email_address_v0@chef.io',
                                   'test_user_v0',
                                   'dddddddddddddddddddddddddddddddd',
                                   '2017-09-06 19:20:35',
                                   false,
                                   '11111111111111111111111111111111'),
                       1,
                       'run the update_user function v0');

PREPARE "update key to null v0" AS SELECT public_key FROM keys
                                     WHERE id='11111111111111111111111111111111';
RETURN QUERY SELECT is_empty(
    '"update key to null v0"',
    'delete public_key in keys table for v0'
);

END;
$$;