-- Add disabled column to users table
-- requires: users

BEGIN;

ALTER TABLE users ADD COLUMN disabled BOOLEAN NOT NULL DEFAULT false;

COMMIT;
