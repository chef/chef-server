-- Deploy enterprise_chef:users_email_functional_index to pg
-- requires: users

CREATE INDEX CONCURRENTLY users_lower_email_idx ON users (lower(email));
