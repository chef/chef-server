BEGIN;

CREATE TABLE IF NOT EXISTS node_policy(
    node_id CHAR(32),
    name VARCHAR(256),
    policy_group VARCHAR(256),
    CONSTRAINT node_policy_node_id_fkey
      FOREIGN KEY(node_id)
      REFERENCES nodes(id)
      ON DELETE CASCADE
);

COMMIT
