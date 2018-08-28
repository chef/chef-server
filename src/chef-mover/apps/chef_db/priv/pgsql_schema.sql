-- Copyright 2011-2018 Chef Software, Inc.
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License.  You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.

--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: checksums; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE checksums (
    org_id character(32) NOT NULL,
    checksum character(32) NOT NULL
);


--
-- Name: clients; Type: TABLE; Schema: public; Owner: -; Tablespace: 
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


--
-- Name: cookbook_version_checksums; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE cookbook_version_checksums (
    cookbook_version_id character(32) NOT NULL,
    org_id character(32) NOT NULL,
    checksum character(32) NOT NULL
);


--
-- Name: cookbook_versions; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE cookbook_versions (
    id character(32) NOT NULL,
    major bigint NOT NULL,
    minor bigint NOT NULL,
    patch bigint NOT NULL,
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

CREATE INDEX cookbook_version_checksums_by_id ON cookbook_version_checksums(cookbook_version_id);

--
-- Name: cookbooks; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE cookbooks (
    id integer NOT NULL,
    org_id character(32) NOT NULL,
    name text NOT NULL,
    authz_id character(32) NOT NULL
);


--
-- Name: cookbook_version_dependencies; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW cookbook_version_dependencies AS
    SELECT c.org_id, c.name, v.major, v.minor, v.patch, v.meta_deps AS dependencies FROM (cookbooks c JOIN cookbook_versions v ON ((c.id = v.cookbook_id)));


--
-- Name: cookbook_versions_by_rank; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW cookbook_versions_by_rank AS
    SELECT v.major, v.minor, v.patch, ((((v.major || '.'::text) || v.minor) || '.'::text) || v.patch) AS version, v.serialized_object, c.org_id, c.name, rank() OVER (PARTITION BY v.cookbook_id ORDER BY v.major DESC, v.minor DESC, v.patch DESC) AS rank FROM (cookbooks c JOIN cookbook_versions v ON ((c.id = v.cookbook_id)));


--
-- Name: cookbooks_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE cookbooks_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: cookbooks_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE cookbooks_id_seq OWNED BY cookbooks.id;


--
-- Name: data_bag_items; Type: TABLE; Schema: public; Owner: -; Tablespace: 
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


--
-- Name: data_bags; Type: TABLE; Schema: public; Owner: -; Tablespace: 
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


--
-- Name: environments; Type: TABLE; Schema: public; Owner: -; Tablespace: 
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


--
-- Name: joined_cookbook_version; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW joined_cookbook_version AS
    SELECT v.major, v.minor, v.patch, ((((v.major || '.'::text) || v.minor) || '.'::text) || v.patch) AS version, v.serialized_object, v.id, c.org_id, c.name FROM (cookbooks c JOIN cookbook_versions v ON ((c.id = v.cookbook_id)));


--
-- Name: nodes; Type: TABLE; Schema: public; Owner: -; Tablespace: 
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
    serialized_object bytea
);
ALTER TABLE ONLY nodes ALTER COLUMN serialized_object SET STORAGE EXTERNAL;


--
-- Name: roles; Type: TABLE; Schema: public; Owner: -; Tablespace: 
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


--
-- Name: sandboxed_checksums; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE sandboxed_checksums (
    org_id character(32) NOT NULL,
    sandbox_id character(32) NOT NULL,
    checksum character(32) NOT NULL,
    created_at timestamp without time zone NOT NULL
);


--
-- Name: schema_info; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE schema_info (
    version integer DEFAULT 0 NOT NULL
);


--
-- Name: osc_users; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE osc_users (
    id character(32) NOT NULL,
    authz_id character(32) NOT NULL,
    username text NOT NULL,
    email text,
    public_key text,
    hashed_password text NOT NULL,
    salt text NOT NULL,
    hash_type text NOT NULL,
    last_updated_by character(32) NOT NULL,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    external_authentication_uid text,
    recovery_authentication_enabled boolean,
    admin boolean DEFAULT false NOT NULL
);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE cookbooks ALTER COLUMN id SET DEFAULT nextval('cookbooks_id_seq'::regclass);


--
-- Name: checksums_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY checksums
    ADD CONSTRAINT checksums_pkey PRIMARY KEY (org_id, checksum);


--
-- Name: clients_authz_id_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY clients
    ADD CONSTRAINT clients_authz_id_key UNIQUE (authz_id);


--
-- Name: clients_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY clients
    ADD CONSTRAINT clients_pkey PRIMARY KEY (id);


--
-- Name: cookbook_versions_cookbook_id_major_minor_patch_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY cookbook_versions
    ADD CONSTRAINT cookbook_versions_cookbook_id_major_minor_patch_key UNIQUE (cookbook_id, major, minor, patch);


--
-- Name: cookbook_versions_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY cookbook_versions
    ADD CONSTRAINT cookbook_versions_pkey PRIMARY KEY (id);


--
-- Name: cookbooks_authz_id_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY cookbooks
    ADD CONSTRAINT cookbooks_authz_id_key UNIQUE (authz_id);


--
-- Name: cookbooks_org_id_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY cookbooks
    ADD CONSTRAINT cookbooks_org_id_name_key UNIQUE (org_id, name);


--
-- Name: cookbooks_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY cookbooks
    ADD CONSTRAINT cookbooks_pkey PRIMARY KEY (id);


--
-- Name: data_bag_items_org_id_data_bag_name_item_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY data_bag_items
    ADD CONSTRAINT data_bag_items_org_id_data_bag_name_item_name_key UNIQUE (org_id, data_bag_name, item_name);


--
-- Name: data_bag_items_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY data_bag_items
    ADD CONSTRAINT data_bag_items_pkey PRIMARY KEY (id);


--
-- Name: data_bags_authz_id_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY data_bags
    ADD CONSTRAINT data_bags_authz_id_key UNIQUE (authz_id);


--
-- Name: data_bags_org_id_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY data_bags
    ADD CONSTRAINT data_bags_org_id_name_key UNIQUE (org_id, name);


--
-- Name: data_bags_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY data_bags
    ADD CONSTRAINT data_bags_pkey PRIMARY KEY (id);


--
-- Name: environments_authz_id_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY environments
    ADD CONSTRAINT environments_authz_id_key UNIQUE (authz_id);


--
-- Name: environments_org_id_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY environments
    ADD CONSTRAINT environments_org_id_name_key UNIQUE (org_id, name);


--
-- Name: environments_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY environments
    ADD CONSTRAINT environments_pkey PRIMARY KEY (id);


--
-- Name: nodes_authz_id_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY nodes
    ADD CONSTRAINT nodes_authz_id_key UNIQUE (authz_id);


--
-- Name: nodes_org_id_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY nodes
    ADD CONSTRAINT nodes_org_id_name_key UNIQUE (org_id, name);


--
-- Name: nodes_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY nodes
    ADD CONSTRAINT nodes_pkey PRIMARY KEY (id);


--
-- Name: org_id_name_unique; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY clients
    ADD CONSTRAINT org_id_name_unique UNIQUE (org_id, name);


--
-- Name: roles_authz_id_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY roles
    ADD CONSTRAINT roles_authz_id_key UNIQUE (authz_id);


--
-- Name: roles_org_id_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY roles
    ADD CONSTRAINT roles_org_id_name_key UNIQUE (org_id, name);


--
-- Name: roles_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY roles
    ADD CONSTRAINT roles_pkey PRIMARY KEY (id);


--
-- Name: sandboxed_checksums_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY sandboxed_checksums
    ADD CONSTRAINT sandboxed_checksums_pkey PRIMARY KEY (sandbox_id, org_id, checksum);


--
-- Name: osc_users_authz_id_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY osc_users
    ADD CONSTRAINT osc_users_authz_id_key UNIQUE (authz_id);


--
-- Name: osc_users_email_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY osc_users
    ADD CONSTRAINT osc_users_email_key UNIQUE (email);


--
-- Name: osc_users_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY osc_users
    ADD CONSTRAINT osc_users_pkey PRIMARY KEY (id);


--
-- Name: osc_users_username_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY osc_users
    ADD CONSTRAINT osc_users_username_key UNIQUE (username);


--
-- Name: cookbooks_org_id_index; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX cookbooks_org_id_index ON cookbooks USING btree (org_id);


--
-- Name: nodes_org_id_environment_index; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX nodes_org_id_environment_index ON nodes USING btree (org_id, environment);


--
-- Name: cookbook_version_checksums_cookbook_version_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY cookbook_version_checksums
    ADD CONSTRAINT cookbook_version_checksums_cookbook_version_id_fkey FOREIGN KEY (cookbook_version_id) REFERENCES cookbook_versions(id);


--
-- Name: cookbook_version_checksums_org_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY cookbook_version_checksums
    ADD CONSTRAINT cookbook_version_checksums_org_id_fkey FOREIGN KEY (org_id, checksum) REFERENCES checksums(org_id, checksum) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- Name: cookbook_versions_cookbook_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY cookbook_versions
    ADD CONSTRAINT cookbook_versions_cookbook_id_fkey FOREIGN KEY (cookbook_id) REFERENCES cookbooks(id) ON DELETE RESTRICT;


--
-- Name: data_bag_items_org_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY data_bag_items
    ADD CONSTRAINT data_bag_items_org_id_fkey FOREIGN KEY (org_id, data_bag_name) REFERENCES data_bags(org_id, name) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- PostgreSQL database dump complete
--

