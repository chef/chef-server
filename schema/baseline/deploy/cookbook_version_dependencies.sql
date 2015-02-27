-- Deploy cookbook_version_dependencies
-- requires: cookbook_versions
-- requires: cookbooks

BEGIN;

CREATE OR REPLACE VIEW cookbook_version_dependencies(
    org_id, -- for filtering
    name, -- cookbook name
    major,
    minor,
    patch,
    dependencies) -- version dependency JSON blob; needed for depsolver
AS
SELECT c.org_id,
       c.name,
       v.major,
       v.minor,
       v.patch,
       v.meta_deps
FROM cookbooks AS c
JOIN cookbook_versions AS v
  ON c.id = v.cookbook_id;

COMMIT;
