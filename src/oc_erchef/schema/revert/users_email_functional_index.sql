-- Revert enterprise_chef:users_email_functional_index from pg

BEGIN;

  DROP INDEX users_lower_email_idx;

COMMIT;
