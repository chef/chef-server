-- Deploy joined_cookbook_version
-- requires: cookbook_versions
-- requires: cookbooks

BEGIN;

CREATE OR REPLACE VIEW joined_cookbook_version(
    -- Cookbook Version fields
    major, -- these 3 are needed for version information (duh)
    minor,
    patch,
    version, -- concatenated string of the complete version
    serialized_object, -- needed to access recipe manifest
    id, -- used for retrieving environment-filtered recipes

    -- Cookbook fields
    org_id, -- used for filtering
    name) -- both version and recipe queries require the cookbook name
AS
SELECT v.major,
       v.minor,
       v.patch,
       v.major || '.' || v.minor || '.' || v.patch,
       v.serialized_object,
       v.id,
       c.org_id,
       c.name
FROM cookbooks AS c
JOIN cookbook_versions AS v
  ON c.id = v.cookbook_id;

COMMIT;
