--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: checksums; Type: TABLE; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

CREATE TABLE checksums (
    org_id character(32) NOT NULL,
    checksum character(32) NOT NULL
);


ALTER TABLE public.checksums OWNER TO "opscode-pgsql";

--
-- Name: clients; Type: TABLE; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

CREATE TABLE clients (
    id character(32) NOT NULL,
    org_id character(32) NOT NULL,
    authz_id character(32) NOT NULL,
    name text NOT NULL,
    public_key text,
    validator boolean NOT NULL,
    last_updated_by character(32) NOT NULL,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    pubkey_version smallint NOT NULL,
    admin boolean DEFAULT false NOT NULL
);


ALTER TABLE public.clients OWNER TO "opscode-pgsql";

--
-- Name: cookbook_version_checksums; Type: TABLE; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

CREATE TABLE cookbook_version_checksums (
    cookbook_version_id character(32) NOT NULL,
    org_id character(32) NOT NULL,
    checksum character(32) NOT NULL
);


ALTER TABLE public.cookbook_version_checksums OWNER TO "opscode-pgsql";

--
-- Name: cookbook_versions; Type: TABLE; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

CREATE TABLE cookbook_versions (
    id character(32) NOT NULL,
    major integer NOT NULL,
    minor integer NOT NULL,
    patch integer NOT NULL,
    frozen boolean NOT NULL,
    meta_attributes bytea NOT NULL,
    meta_deps text NOT NULL,
    meta_long_desc bytea NOT NULL,
    metadata bytea NOT NULL,
    serialized_object bytea NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    created_at timestamp without time zone NOT NULL,
    last_updated_by character(32) NOT NULL,
    cookbook_id integer NOT NULL
);
ALTER TABLE ONLY cookbook_versions ALTER COLUMN meta_attributes SET STORAGE EXTERNAL;
ALTER TABLE ONLY cookbook_versions ALTER COLUMN meta_long_desc SET STORAGE EXTERNAL;
ALTER TABLE ONLY cookbook_versions ALTER COLUMN metadata SET STORAGE EXTERNAL;
ALTER TABLE ONLY cookbook_versions ALTER COLUMN serialized_object SET STORAGE EXTERNAL;


ALTER TABLE public.cookbook_versions OWNER TO "opscode-pgsql";

--
-- Name: cookbooks; Type: TABLE; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

CREATE TABLE cookbooks (
    id integer NOT NULL,
    org_id character(32) NOT NULL,
    name text NOT NULL,
    authz_id character(32) NOT NULL
);


ALTER TABLE public.cookbooks OWNER TO "opscode-pgsql";

--
-- Name: cookbook_version_dependencies; Type: VIEW; Schema: public; Owner: opscode-pgsql
--

CREATE VIEW cookbook_version_dependencies AS
    SELECT c.org_id, c.name, v.major, v.minor, v.patch, v.meta_deps AS dependencies FROM (cookbooks c JOIN cookbook_versions v ON ((c.id = v.cookbook_id)));


ALTER TABLE public.cookbook_version_dependencies OWNER TO "opscode-pgsql";

--
-- Name: cookbook_versions_by_rank; Type: VIEW; Schema: public; Owner: opscode-pgsql
--

CREATE VIEW cookbook_versions_by_rank AS
    SELECT v.major, v.minor, v.patch, ((((v.major || '.'::text) || v.minor) || '.'::text) || v.patch) AS version, v.serialized_object, c.org_id, c.name, rank() OVER (PARTITION BY v.cookbook_id ORDER BY v.major DESC, v.minor DESC, v.patch DESC) AS rank FROM (cookbooks c JOIN cookbook_versions v ON ((c.id = v.cookbook_id)));


ALTER TABLE public.cookbook_versions_by_rank OWNER TO "opscode-pgsql";

--
-- Name: cookbooks_id_seq; Type: SEQUENCE; Schema: public; Owner: opscode-pgsql
--

CREATE SEQUENCE cookbooks_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.cookbooks_id_seq OWNER TO "opscode-pgsql";

--
-- Name: cookbooks_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: opscode-pgsql
--

ALTER SEQUENCE cookbooks_id_seq OWNED BY cookbooks.id;


--
-- Name: data_bag_items; Type: TABLE; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

CREATE TABLE data_bag_items (
    id character(32) NOT NULL,
    org_id character(32) NOT NULL,
    data_bag_name text NOT NULL,
    item_name text NOT NULL,
    last_updated_by character(32) NOT NULL,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    serialized_object bytea
);
ALTER TABLE ONLY data_bag_items ALTER COLUMN serialized_object SET STORAGE EXTERNAL;


ALTER TABLE public.data_bag_items OWNER TO "opscode-pgsql";

--
-- Name: data_bags; Type: TABLE; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

CREATE TABLE data_bags (
    id character(32) NOT NULL,
    authz_id character(32) NOT NULL,
    org_id character(32) NOT NULL,
    name text NOT NULL,
    last_updated_by character(32) NOT NULL,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL
);


ALTER TABLE public.data_bags OWNER TO "opscode-pgsql";

--
-- Name: environments; Type: TABLE; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

CREATE TABLE environments (
    id character(32) NOT NULL,
    authz_id character(32) NOT NULL,
    org_id character(32) NOT NULL,
    name text NOT NULL,
    last_updated_by character(32) NOT NULL,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    serialized_object bytea
);
ALTER TABLE ONLY environments ALTER COLUMN serialized_object SET STORAGE EXTERNAL;


ALTER TABLE public.environments OWNER TO "opscode-pgsql";

--
-- Name: joined_cookbook_version; Type: VIEW; Schema: public; Owner: opscode-pgsql
--

CREATE VIEW joined_cookbook_version AS
    SELECT v.major, v.minor, v.patch, ((((v.major || '.'::text) || v.minor) || '.'::text) || v.patch) AS version, v.serialized_object, v.id, c.org_id, c.name FROM (cookbooks c JOIN cookbook_versions v ON ((c.id = v.cookbook_id)));


ALTER TABLE public.joined_cookbook_version OWNER TO "opscode-pgsql";

--
-- Name: nodes; Type: TABLE; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

CREATE TABLE nodes (
    id character(32) NOT NULL,
    authz_id character(32) NOT NULL,
    org_id character(32) NOT NULL,
    name text NOT NULL,
    environment text NOT NULL,
    last_updated_by character(32) NOT NULL,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    serialized_object text
);


ALTER TABLE public.nodes OWNER TO "opscode-pgsql";

--
-- Name: roles; Type: TABLE; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

CREATE TABLE roles (
    id character(32) NOT NULL,
    authz_id character(32) NOT NULL,
    org_id character(32) NOT NULL,
    name text NOT NULL,
    last_updated_by character(32) NOT NULL,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    serialized_object bytea
);
ALTER TABLE ONLY roles ALTER COLUMN serialized_object SET STORAGE EXTERNAL;


ALTER TABLE public.roles OWNER TO "opscode-pgsql";

--
-- Name: sandboxed_checksums; Type: TABLE; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

CREATE TABLE sandboxed_checksums (
    org_id character(32) NOT NULL,
    sandbox_id character(32) NOT NULL,
    checksum character(32) NOT NULL,
    created_at timestamp without time zone NOT NULL
);


ALTER TABLE public.sandboxed_checksums OWNER TO "opscode-pgsql";

--
-- Name: schema_info; Type: TABLE; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

CREATE TABLE schema_info (
    version integer DEFAULT 0 NOT NULL
);


ALTER TABLE public.schema_info OWNER TO "opscode-pgsql";

--
-- Name: users; Type: TABLE; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

CREATE TABLE users (
    id character(32) NOT NULL,
    authz_id character(32) NOT NULL,
    username text NOT NULL,
    email text,
    pubkey_version integer NOT NULL,
    public_key text,
    serialized_object text,
    last_updated_by character(32) NOT NULL,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    external_authentication_uid text,
    recovery_authentication_enabled boolean,
    admin boolean DEFAULT false NOT NULL
);


ALTER TABLE public.users OWNER TO "opscode-pgsql";

--
-- Name: id; Type: DEFAULT; Schema: public; Owner: opscode-pgsql
--

ALTER TABLE cookbooks ALTER COLUMN id SET DEFAULT nextval('cookbooks_id_seq'::regclass);


--
-- Name: checksums_pkey; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY checksums
    ADD CONSTRAINT checksums_pkey PRIMARY KEY (org_id, checksum);


--
-- Name: clients_authz_id_key; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY clients
    ADD CONSTRAINT clients_authz_id_key UNIQUE (authz_id);


--
-- Name: clients_pkey; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY clients
    ADD CONSTRAINT clients_pkey PRIMARY KEY (id);


--
-- Name: cookbook_versions_cookbook_id_major_minor_patch_key; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY cookbook_versions
    ADD CONSTRAINT cookbook_versions_cookbook_id_major_minor_patch_key UNIQUE (cookbook_id, major, minor, patch);


--
-- Name: cookbook_versions_pkey; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY cookbook_versions
    ADD CONSTRAINT cookbook_versions_pkey PRIMARY KEY (id);


--
-- Name: cookbooks_authz_id_key; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY cookbooks
    ADD CONSTRAINT cookbooks_authz_id_key UNIQUE (authz_id);


--
-- Name: cookbooks_org_id_name_key; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY cookbooks
    ADD CONSTRAINT cookbooks_org_id_name_key UNIQUE (org_id, name);


--
-- Name: cookbooks_pkey; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY cookbooks
    ADD CONSTRAINT cookbooks_pkey PRIMARY KEY (id);


--
-- Name: data_bag_items_org_id_data_bag_name_item_name_key; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY data_bag_items
    ADD CONSTRAINT data_bag_items_org_id_data_bag_name_item_name_key UNIQUE (org_id, data_bag_name, item_name);


--
-- Name: data_bag_items_pkey; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY data_bag_items
    ADD CONSTRAINT data_bag_items_pkey PRIMARY KEY (id);


--
-- Name: data_bags_authz_id_key; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY data_bags
    ADD CONSTRAINT data_bags_authz_id_key UNIQUE (authz_id);


--
-- Name: data_bags_org_id_name_key; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY data_bags
    ADD CONSTRAINT data_bags_org_id_name_key UNIQUE (org_id, name);


--
-- Name: data_bags_pkey; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY data_bags
    ADD CONSTRAINT data_bags_pkey PRIMARY KEY (id);


--
-- Name: environments_authz_id_key; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY environments
    ADD CONSTRAINT environments_authz_id_key UNIQUE (authz_id);


--
-- Name: environments_org_id_name_key; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY environments
    ADD CONSTRAINT environments_org_id_name_key UNIQUE (org_id, name);


--
-- Name: environments_pkey; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY environments
    ADD CONSTRAINT environments_pkey PRIMARY KEY (id);


--
-- Name: nodes_authz_id_key; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY nodes
    ADD CONSTRAINT nodes_authz_id_key UNIQUE (authz_id);


--
-- Name: nodes_org_id_name_key; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY nodes
    ADD CONSTRAINT nodes_org_id_name_key UNIQUE (org_id, name);


--
-- Name: nodes_pkey; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY nodes
    ADD CONSTRAINT nodes_pkey PRIMARY KEY (id);


--
-- Name: org_id_name_unique; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY clients
    ADD CONSTRAINT org_id_name_unique UNIQUE (org_id, name);


--
-- Name: roles_authz_id_key; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY roles
    ADD CONSTRAINT roles_authz_id_key UNIQUE (authz_id);


--
-- Name: roles_org_id_name_key; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY roles
    ADD CONSTRAINT roles_org_id_name_key UNIQUE (org_id, name);


--
-- Name: roles_pkey; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY roles
    ADD CONSTRAINT roles_pkey PRIMARY KEY (id);


--
-- Name: sandboxed_checksums_pkey; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY sandboxed_checksums
    ADD CONSTRAINT sandboxed_checksums_pkey PRIMARY KEY (sandbox_id, org_id, checksum);


--
-- Name: users_authz_id_key; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_authz_id_key UNIQUE (authz_id);


--
-- Name: users_email_key; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_email_key UNIQUE (email);


--
-- Name: users_pkey; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: users_username_key; Type: CONSTRAINT; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_username_key UNIQUE (username);


--
-- Name: cookbooks_org_id_index; Type: INDEX; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

CREATE INDEX cookbooks_org_id_index ON cookbooks USING btree (org_id);


--
-- Name: nodes_org_id_environment_index; Type: INDEX; Schema: public; Owner: opscode-pgsql; Tablespace: 
--

CREATE INDEX nodes_org_id_environment_index ON nodes USING btree (org_id, environment);


--
-- Name: cookbook_version_checksums_cookbook_version_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: opscode-pgsql
--

ALTER TABLE ONLY cookbook_version_checksums
    ADD CONSTRAINT cookbook_version_checksums_cookbook_version_id_fkey FOREIGN KEY (cookbook_version_id) REFERENCES cookbook_versions(id);


--
-- Name: cookbook_version_checksums_org_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: opscode-pgsql
--

ALTER TABLE ONLY cookbook_version_checksums
    ADD CONSTRAINT cookbook_version_checksums_org_id_fkey FOREIGN KEY (org_id, checksum) REFERENCES checksums(org_id, checksum) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- Name: cookbook_versions_cookbook_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: opscode-pgsql
--

ALTER TABLE ONLY cookbook_versions
    ADD CONSTRAINT cookbook_versions_cookbook_id_fkey FOREIGN KEY (cookbook_id) REFERENCES cookbooks(id) ON DELETE RESTRICT;


--
-- Name: data_bag_items_org_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: opscode-pgsql
--

ALTER TABLE ONLY data_bag_items
    ADD CONSTRAINT data_bag_items_org_id_fkey FOREIGN KEY (org_id, data_bag_name) REFERENCES data_bags(org_id, name) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: public; Type: ACL; Schema: -; Owner: opscode-pgsql
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM "opscode-pgsql";
GRANT ALL ON SCHEMA public TO "opscode-pgsql";
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

