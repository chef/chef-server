-- Testing Data

INSERT INTO auth_group(authz_id)
VALUES
       ('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'),
       ('bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'),
       ('cccccccccccccccccccccccccccccccc');

INSERT INTO auth_actor(authz_id)
VALUES
       ('zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz'),
       ('yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy'),
       ('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'),
       ('wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww');

INSERT INTO group_group_relations(parent, child)
VALUES
       (group_id('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'), group_id('bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb')),
       (group_id('bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'), group_id('cccccccccccccccccccccccccccccccc'));

INSERT INTO group_actor_relations(parent, child)
VALUES
       (group_id('bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'), actor_id('yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy')),
       (group_id('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'), actor_id('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')),
       (group_id('cccccccccccccccccccccccccccccccc'), actor_id('wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww'));

INSERT INTO auth_object(authz_id)
VALUES
        ('oooooooooooooooooooooooooooooooo');

INSERT INTO object_acl_group(target, authorizee, permission)
VALUES
        (object_id('oooooooooooooooooooooooooooooooo'), group_id('bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'), 'read'),
        (object_id('oooooooooooooooooooooooooooooooo'), group_id('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'), 'delete');

INSERT INTO object_acl_actor(target, authorizee, permission)
VALUES
        (object_id('oooooooooooooooooooooooooooooooo'), actor_id('zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz'), 'update'),
        (object_id('oooooooooooooooooooooooooooooooo'), actor_id('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'), 'delete');

INSERT INTO auth_container(authz_id)
VALUES ('nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn');

INSERT INTO container_acl_actor(target, authorizee, permission)
VALUES (container_id('nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn'),
        actor_id('zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz'), 'create');

INSERT INTO auth_group(authz_id)
VALUES ('gggggggggggggggggggggggggggggggg');

INSERT INTO group_acl_actor(target, authorizee, permission)
VALUES (group_id('gggggggggggggggggggggggggggggggg'),
        actor_id('zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz'), 'read');

INSERT INTO auth_actor(authz_id)
VALUES ('tttttttttttttttttttttttttttttttt');

INSERT INTO actor_acl_actor(target, authorizee, permission)
VALUES (actor_id('tttttttttttttttttttttttttttttttt'),
        actor_id('zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz'), 'delete');
