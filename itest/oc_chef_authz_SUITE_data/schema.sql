CREATE TABLE containers(
       id VARCHAR(36) PRIMARY KEY,
       authz_id CHAR(32) NOT NULL UNIQUE,
       org_id CHAR(32) NOT NULL,
       name TEXT NOT NULL,
       CONSTRAINT org_id_name_unique UNIQUE(org_id, name),
       last_updated_by CHAR(32) NOT NULL,
       created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL,
       updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL
);

INSERT INTO containers
  VALUES ('11110000000000000000000000000000',
          '22220000000000000000000000000000',
          '33330000000000000000000000000000',
          'nodes',
          '44440000000000000000000000000000',
          '2004-10-19 10:23:54',
          '2004-10-19 10:23:54'
  );

CREATE TABLE groups (
    id character varying(36) NOT NULL,
    authz_id character(32) NOT NULL,
    org_id character(32) NOT NULL,
    name text NOT NULL,
    last_updated_by character(32) NOT NULL,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL
);


INSERT INTO groups
  VALUES ('55550000000000000000000000000000',
          '66660000000000000000000000000000',
          '77770000000000000000000000000000',
          'admins',
          '88880000000000000000000000000000',
          '1989-11-09 10:45:00',
          '1989-11-09 10:45:00'
  );
