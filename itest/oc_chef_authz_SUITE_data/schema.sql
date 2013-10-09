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
  VALUES ('1111e9532d61a632f2a431b0299198b4',
          '2222224947d7ed92e872e53b620e94b7',
          '3333224947d7ed92e872e53b620e94b7',
          'nodes',
          '4444224947d7ed92e872e53b620e94b7',
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
  VALUES ('5555e9532d61a632f2a431b0299198b4',
          '6666224947d7ed92e872e53b620e94b7',
          '7777224947d7ed92e872e53b620e94b7',
          'admins',
          '8888224947d7ed92e872e53b620e94b7',
          '1989-11-09 10:45:00',
          '1989-11-09 10:45:00'
  );
