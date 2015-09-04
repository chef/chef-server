-- Deploy node_policy_name_policy_group

BEGIN;

ALTER TABLE nodes ADD COLUMN policy_group VARCHAR(255);
ALTER TABLE nodes ADD COLUMN policy_name VARCHAR(255);

CREATE INDEX nodes_policy_group ON nodes(org_id, policy_group);
CREATE INDEX nodes_policy_name ON nodes(org_id, policy_name);

COMMIT;
