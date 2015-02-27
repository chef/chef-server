-- Verify insert_cookbook_artifact_version

BEGIN;

SELECT has_function_privilege(
  'insert_cookbook_artifact_version(varchar(255),
                                    bytea,
                                    bytea,
                                    timestamp without time zone,
                                    char(32),
                                    char(32),
                                    text,
                                    char(32),
                                    char(32)[])',
  'execute');

ROLLBACK;
