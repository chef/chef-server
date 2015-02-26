
-- Design note
-- Users and clients are separate tables, but a single keys table makes more sense. Unfortunately we can't do
-- foreign key constraints against two tables. Instead we create the effect of 'ON DELETE CASCADE' via a trigger.
-- However that doesn't prohibit orphaned keys in the table, as we can insert keys without having a matching user or client.
--
-- As an alternate implmentation, could split keys into two tables, client_keys, and user_keys
-- We'd want to add a FK constraint to the id field 'REFERENCES clients(id) ON DELETE CASCADE'
--

BEGIN;

CREATE TABLE keys(
   id char(32) NOT NULL,
   key_name text NOT NULL,
   public_key text NOT NULL,
   key_version integer NOT NULL,
   created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
   expires_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
   PRIMARY KEY (id, key_name)
   );

--
-- This should simplify lookup in the authorization check function.
-- Authz id is added to make this a one stop query.
--
-- This needs some benchmarking on real data sizes; this may trigger sequential scans
-- when we do something like
-- select id,org_id,name,type from keys_by_name where name ='wei' AND (org_id='global' OR org_id='181131e4e896ec930f5ca3f36b490c1f');;
--
CREATE OR REPLACE view keys_by_name AS
SELECT id, org_id, name, authz_id, 'client' AS type, key_name, keys.public_key, key_version, expires_at FROM clients INNER JOIN keys USING (id)
UNION ALL
SELECT id, 'global', username AS name, authz_id, 'user' AS type, key_name, keys.public_key, key_version, expires_at FROM users INNER JOIN keys USING (id);

COMMIT;
